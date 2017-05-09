from markdown.preprocessors import Preprocessor
from markdown.extensions import Extension
from markdown.blockprocessors import BlockProcessor

import subprocess

import sqlparse

import md5

import os
import os.path

def run_example(template_path, cache_dir, example_lines):
    template_data = []
    options = {}
    with open(template_path) as f:
        for line in f:
            line = line.rstrip()
            if line.startswith("-- !"):
                variable = line[5:]
                key,value = variable.split(":")
                options[key.strip()] = value.strip()
            elif line.strip() == "BEAM_PLACEHOLDER":
                idx = line.find("BEAM_PLACEHOLDER")
                whitespace = line[:idx]
                for example_line in example_lines:
                    template_data.append(whitespace + example_line.rstrip())
            else:
                template_data.append(line)

    build_dir = options.get('BUILD_DIR', '.')
    build_command = options.get('BUILD_COMMAND')
    extra_deps = options.get('EXTRA_DEPS', "").split()

    lines_hash = md5.md5("$$TEMPLATEPATH$$" + template_path +
                         "".join(example_lines))
    with open(template_path) as f:
        for line in f:
            lines_hash.update(line)

    for extra_dep in extra_deps:
        extra_dep = os.path.join(os.path.dirname(template_path), extra_dep)
        lines_hash.update("EXTRA_DEP: %s" % extra_dep)
        with open(extra_dep) as f:
            for line in f:
                lines_hash.update(line)

    lines_hash = lines_hash.hexdigest()
    if os.path.exists(os.path.join(cache_dir, lines_hash)):
        with open(os.path.join(cache_dir, lines_hash)) as cached:
            return [x.rstrip() for x in cached]

    print "run_example", template_path, example_lines
    if build_command is None:
        return ["No BUILD_COMMAND specified"] + example_lines

    proc = subprocess.Popen(build_command, shell=True, cwd=os.path.abspath(build_dir), close_fds=True,
                            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    (out, err) = proc.communicate("\n".join(template_data))

    retcode = proc.wait()

    print out, err
    if retcode == 0:
        out = sqlparse.format(out, reindent=True)
        with open(os.path.join(cache_dir, lines_hash), 'wt') as f:
            f.write(out)
        return out.split("\n")
    else:
        return err.split()

class BeamQueryBlockProcessor(Preprocessor):
    def __init__(self, *args, **kwargs):
        self.template_dir = kwargs['template_dir']
        self.cache_dir    = kwargs['cache_dir']
        del kwargs['template_dir']
        del kwargs['cache_dir']

        if not os.path.exists(self.cache_dir):
            os.makedirs(self.cache_dir)

        super(BeamQueryBlockProcessor, self).__init__(*args, **kwargs)

    def run(self, lines):
        query_mode = None
        beam_templates = []
        cur_query = []
        output = []

        def do_consume(line):
            if line.startswith("```"):
                output.append("```haskell")
                output.extend(cur_query)
                output.append("```")

                for (template, syntax) in beam_templates:
                    template_path = os.path.join(self.template_dir, template + ".hs")
                    template_data = run_example(template_path, self.cache_dir, cur_query)

                    output.append("```%s" % syntax)
                    output.extend(template_data)
                    output.append("```")
                return None
            else:
                cur_query.append(line)
                return query_mode

        for line in lines:
            if query_mode == 'start':
                if line.startswith("```haskell"):
                    cur_query = []
                    query_mode = 'get_template'
                    beam_templates = []
                else:
                    query_mode = None
            elif query_mode == 'get_template':
                if line.startswith("!"):
                    beam_templates.append(line[1:].split()[0:2])
                else:
                    query_mode = 'consume'
                    query_mode = do_consume(line)
            elif query_mode == 'consume':
                query_mode = do_consume(line)
            else:
                if line.startswith("!beam-query"):
                    query_mode = 'start'
                    cur_query = []
                    beam_templates = []
                else:
                    output.append(line)
        return output


class BeamQueryExtension(Extension):
    config = { 'template_dir' : [".", "Directory for template files"],
               'cache_dir'    : ["./.beam-query-cache", "Directory for cached results"] }
    def extendMarkdown(self, md, md_globals):

        md.preprocessors.add('beam-query',
                             BeamQueryBlockProcessor(md, template_dir=os.path.abspath(self.getConfig('template_dir')),
                                                     cache_dir=os.path.abspath(self.getConfig('cache_dir'))),
                             "_begin")

def makeExtension(*args, **kwargs):
    return BeamQueryExtension(*args, **kwargs)
