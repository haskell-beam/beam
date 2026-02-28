from markdown.preprocessors import Preprocessor
from markdown.extensions import Extension

import subprocess
import sqlparse
import hashlib
import os
import os.path
import yaml
import json
import urllib.request
import zipfile
import sys


def check_ci():
    return os.environ.get("CI", "false") == "true" and "BEAM_DOC_BACKEND" in os.environ


def fetch_backend_src(backend_name, cache_dir, base_dir, src):
    if "file" in src:
        return os.path.join(base_dir, src["file"])
    elif "local" in src:
        return os.path.join(base_dir, src["local"])
    elif "github" in src:
        repo_name = "/".join(src["github"].split("/")[1:])
        github_archive_name = f"{repo_name}-{src['revision']}"
        backend_dir = os.path.join(
            cache_dir, "backends", f"{backend_name}-{src['revision']}", github_archive_name
        )

        if not os.path.exists(backend_dir):
            github_url = f"https://github.com/{src['github']}/archive/{src.get('revision', 'master')}.zip"
            print(f"Downloading beam backend {backend_name} from {github_url}")

            local_file = os.path.join(cache_dir, f"{backend_name}_github.zip")
            urllib.request.urlretrieve(github_url, local_file)

            print("Verifying archive...")

            h = hashlib.sha256()
            with open(local_file, "rb") as f:
                for chunk in iter(lambda: f.read(4096 * 4), b""):
                    h.update(chunk)
            assert h.hexdigest() == src["sha256"], (
                f"Invalid checksum, expected {src['sha256']}, got {h.hexdigest()}"
            )

            archive = zipfile.ZipFile(local_file)
            os.makedirs(backend_dir)
            archive.extractall(os.path.dirname(backend_dir))

        return backend_dir
    else:
        print("Invalid source spec", src)
        sys.exit(1)


def setup_backend(cache_dir, base_dir, backend):
    src = backend["src"]
    src_dir = fetch_backend_src(backend["backend-name"], cache_dir, base_dir, src)

    backend_script = os.path.join(src_dir, "beam-docs.sh")
    backend_cmd = f"sh {backend_script}"

    opts = backend.get("backend-options", "")

    opts_hash = hashlib.md5(opts.encode("utf-8")).hexdigest()
    status_file = os.path.join(cache_dir, f"{backend['backend-name']}-{opts_hash}")

    if not os.path.exists(status_file):
        print(
            "bash environment is",
            ["env", "bash", "-c", f"{backend_cmd} {opts}"],
            os.path.join(base_dir, "docs/beam-docs-library.sh"),
        )
        setup_cmd = subprocess.Popen(
            ["env", "bash", "-c", f"{backend_cmd} {opts}"],
            cwd=os.path.abspath(cache_dir),
            close_fds=True,
            stdout=subprocess.PIPE,
            env=dict(
                os.environ,
                BEAM_DOCS_LIBRARY=os.path.join(base_dir, "docs/beam-docs-library.sh"),
            ),
        )
        (out, _) = setup_cmd.communicate()
        retcode = setup_cmd.wait()
        if retcode == 0:
            print(f"Successfully setup backend {backend['backend-name']}")
            out = out.decode("utf-8")
            with open(status_file, "wt") as f:
                f.write(out)
            return out
        else:
            print(out)
            sys.exit(1)
    else:
        with open(status_file, "rt") as f:
            return f.read()


def backend_match_reqs(backend, reqs):
    backend_features = backend["supports"]
    for req in reqs:
        if req.startswith("!on:"):
            not_backend = req[4:]
            if not_backend == backend["backend-name"]:
                return False
        elif req.startswith("!") and req[1:] in backend_features:
            return False
        elif req.startswith("only:"):
            exp_backend = req[5:]
            if exp_backend != backend["backend-name"]:
                return False
        elif req not in backend_features:
            return False
    return True


def read_template(template_path, subst_vars):
    template_data = []
    options = {}
    with open(template_path) as f:
        for line in f:
            line = line.rstrip()
            if line.startswith("-- !"):
                variable = line[5:]
                key, value = variable.split(":")
                options[key.strip()] = value.strip()
            elif line.strip().startswith("BEAM_"):
                beam_var = line.strip()[5:]
                idx = line.find(line.strip())
                whitespace = line[:idx]
                if beam_var in subst_vars:
                    for example_line in subst_vars[beam_var]:
                        template_data.append(whitespace + example_line.rstrip())
                else:
                    template_data.append(line)
            else:
                template_data.append(line)
    return template_data, options


def hash_template(extra_data, template_path, extra_deps):
    lines_hash = hashlib.md5(extra_data)
    with open(template_path) as f:
        for line in f:
            lines_hash.update(line.encode("utf-8"))

    for extra_dep in extra_deps:
        extra_dep = os.path.join(os.path.dirname(template_path), extra_dep)
        lines_hash.update(f"EXTRA_DEP: {extra_dep}".encode("utf-8"))
        with open(extra_dep) as f:
            for line in f:
                lines_hash.update(line.encode("utf-8"))

    return lines_hash.hexdigest()


def find_cached_file(cache_dir, lines_hash):
    path = os.path.join(cache_dir, lines_hash)
    if os.path.exists(path):
        with open(path) as cached:
            return [x.rstrip() for x in cached]
    return None


def save_cached_file(cache_dir, lines_hash, out):
    with open(os.path.join(cache_dir, lines_hash), "wt") as f:
        f.write(out)


def run_backend_example(backend, template, cache_dir, base_dir, full_example_lines):
    backend_haskell_names = backend["haskell-names"]
    module = backend["backend-module"]
    mnemonic = backend_haskell_names["mnemonic"]
    with_database_debug = f"{mnemonic}.{backend_haskell_names['with-database-debug']}"
    select_syntax = f"{mnemonic}.{backend_haskell_names['select-syntax']}"
    backend_type = f"{mnemonic}.{backend_haskell_names['backend']}"
    backend_monad = f"{mnemonic}.{backend_haskell_names['monad']}"
    extra_imports = backend.get("extra-imports", [])

    example_lines = []
    for line in full_example_lines:
        if line.startswith("--! import"):
            extra_imports.append(line[len("--! import"):])
        else:
            example_lines.append(line)

    open_db_data = setup_backend(cache_dir, base_dir, backend)

    template_data, options = read_template(
        template,
        {
            "PLACEHOLDER": example_lines,
            "MODULE_IMPORT": [f"import qualified {module} as {mnemonic}"]
            + [f"import {mod}" for mod in extra_imports],
            "BACKEND_EXTRA": backend.get("backend-extra", "").split("\n"),
            "OPEN_DATABASE": open_db_data.split("\n"),
        },
    )

    packages = ["beam-core", backend_haskell_names["package"]] + backend.get("extra-packages", [])
    packages = [f"-package {pkgname}" for pkgname in packages]
    decl_options = options.get("BUILD_OPTIONS", "").replace("$$BEAM_SOURCE$$", base_dir)
    build_options = (
        f" -XCPP -DBEAM_BACKEND={backend_type}"
        f" -DBEAM_BACKEND_MONAD={backend_monad}"
        f" -DBEAM_WITH_DATABASE_DEBUG={with_database_debug} "
        + decl_options
    )
    extra_deps = options.get("EXTRA_DEPS", "").split()
    output_format = options.get("OUTPUT_FORMAT", "sql")

    lines_hash = hash_template(
        b"$$TEMPLATEPATH$$"
        + template.encode("ascii")
        + "".join(example_lines).encode("ascii", "xmlcharrefreplace")
        + json.dumps(backend).encode("utf-8"),
        template,
        extra_deps,
    )

    cached_data = find_cached_file(cache_dir, lines_hash)
    if cached_data is not None:
        return cached_data

    source_file = os.path.join(os.path.abspath(cache_dir), f"{lines_hash}.hs")
    binary_file = os.path.join(os.path.abspath(cache_dir), f"{lines_hash}")
    with open(source_file, "wt") as source_hdl:
        source_hdl.write("\n".join(template_data))

    compile_command = f"runhaskell {build_options} {source_file}"
    print(f"Running backend example {lines_hash}: {compile_command}")
    is_ci = check_ci()
    proc = subprocess.Popen(
        compile_command,
        shell=True,
        cwd=os.path.abspath(cache_dir),
        close_fds=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE if not is_ci else None,
    )

    (out, err) = proc.communicate()
    out = out.decode("utf-8")

    retcode = proc.wait()
    print(f"Ran backend example {lines_hash}", file=sys.stderr)
    if retcode == 0:
        os.remove(source_file)
        # Clean up compilation artifacts
        for ext in ["", ".hi", ".o"]:
            path = binary_file + ext if ext else binary_file
            if os.path.exists(path):
                os.remove(path)
        if output_format == "sql":
            out = sqlparse.format(out, reindent=True)
        save_cached_file(cache_dir, lines_hash, out)
        return out.split("\n")
    else:
        if is_ci:
            print(f"Error in source file {source_file}")
            print("Example is\n", "\n".join(example_lines))
            print("--- Generated source ---")
            with open(source_file) as dbg:
                for i, line in enumerate(dbg, 1):
                    print(f"{i:4d} | {line}", end="")
            print("--- End generated source ---")
            sys.exit(1)
        else:
            print(f"Error in source file {source_file}")
            sys.stderr.flush()
            sys.stderr.buffer.write(err)
            err = err.decode("utf-8")
            return err.split()


def run_example(template_path, cache_dir, example_lines):
    template_data, options = read_template(template_path, {"PLACEHOLDER": example_lines})

    build_dir = options.get("BUILD_DIR", ".")
    build_command = options.get("BUILD_COMMAND")
    extra_deps = options.get("EXTRA_DEPS", "").split()
    out_format = options.get("FORMAT", "sql")

    lines_hash = hash_template(
        b"$$TEMPLATEPATH$$"
        + template_path.encode("utf-8")
        + "".join(example_lines).encode("ascii", "xmlcharrefreplace"),
        template_path,
        extra_deps,
    )
    cached_data = find_cached_file(cache_dir, lines_hash)
    if cached_data is not None:
        return cached_data

    if build_command is None:
        return ["No BUILD_COMMAND specified"] + example_lines

    source_content = "\n".join(template_data)
    needs_th = "TemplateHaskell" in source_content or "makeLenses" in source_content

    source_file = os.path.join(os.path.abspath(cache_dir), f"{lines_hash}.hs")
    binary_file = os.path.join(os.path.abspath(cache_dir), f"{lines_hash}")
    with open(source_file, "wt") as f:
        f.write(source_content)

    # Note: `runhaskell` is much faster than `ghc --make`, BUT does not support
    # template-haskell. Therefore, if we detect that there's some template Haskell
    # in a source file (`needs_th`), we switch the build command to a much slower method.
    if build_command.startswith("runhaskell"):
        ghc_args = build_command[len("runhaskell"):]
        if needs_th:
            compile_command = f"ghc --make -O0 -o {binary_file} {ghc_args} {source_file} && {binary_file}"
        else:
            compile_command = f"runhaskell {ghc_args} {source_file}"
    else:
        compile_command = f"{build_command} {source_file}"

    print(f"Running example {lines_hash}: {compile_command}")
    is_ci = check_ci()
    proc = subprocess.Popen(
        compile_command,
        shell=True,
        cwd=os.path.abspath(build_dir),
        close_fds=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE if not is_ci else None,
    )

    (out, err) = proc.communicate()
    out = out.decode("utf-8")

    retcode = proc.wait()

    print(f"Ran example {lines_hash}", file=sys.stderr)
    if retcode == 0:
        os.remove(source_file)
        # Clean up compilation artifacts
        for ext in ["", ".hi", ".o"]:
            path = binary_file + ext if ext else binary_file
            if os.path.exists(path):
                os.remove(path)
        if out_format == "sql":
            out = sqlparse.format(out, reindent=True)
        save_cached_file(cache_dir, lines_hash, out)
        return out.split("\n")
    else:
        if is_ci:
            print(f"Error in source file {source_file}")
            print(f"Error processing file {lines_hash}")
            print("Example is\n", "\n".join(example_lines))
            print("--- Generated source ---")
            with open(source_file) as dbg:
                for i, line in enumerate(dbg, 1):
                    print(f"{i:4d} | {line}", end="")
            print("--- End generated source ---")
            sys.exit(1)
        else:
            sys.stdout.flush()
            sys.stderr.flush()
            sys.stderr.buffer.write(err)
            err = err.decode("utf-8")
            return err.split()


class BeamQueryBlockProcessor(Preprocessor):
    def __init__(self, md, *, template_dir, cache_dir, backends, base_dir):
        self.template_dir = template_dir
        self.cache_dir = cache_dir
        self.backends = backends
        self.base_dir = base_dir

        if not os.path.exists(self.cache_dir):
            os.makedirs(self.cache_dir)

        super().__init__(md)

    def run(self, lines):
        query_mode = None
        beam_templates = []
        beam_template = None
        cur_query = []
        output = []

        is_example = False
        using_backends = []

        def do_consume(line):
            if line.startswith("```"):
                output.append("```haskell")
                output.extend(cur_query)
                output.append("```")

                if is_example:
                    for backend_name in using_backends:
                        backend = self.backends[backend_name]
                        template_path = os.path.join(self.template_dir, f"{beam_template}.hs")
                        template_data = run_backend_example(
                            backend, template_path, self.cache_dir, self.base_dir, cur_query
                        )
                        output.append(f"```sql name={backend['backend-name']}")
                        output.extend(template_data)
                        output.append("```")
                else:
                    for template, name, syntax in beam_templates:
                        template_path = os.path.join(self.template_dir, f"{template}.hs")
                        template_data = run_example(template_path, self.cache_dir, cur_query)
                        output.append(f"```{syntax} name={name}")
                        output.extend(template_data)
                        output.append("```")
                return None
            else:
                cur_query.append(line)
                return query_mode

        for line in lines:
            if query_mode == "start":
                if line.startswith("```haskell"):
                    cur_query = []
                    query_mode = "get_template"
                    beam_templates = []
                else:
                    query_mode = None
            elif query_mode == "get_template":
                if line.startswith("!example "):
                    is_example = True

                    tmpl_and_reqs = line[9:].split(" ")
                    tmpl = tmpl_and_reqs[0]
                    reqs = tmpl_and_reqs[1:]

                    using_backends = [
                        be
                        for be in self.backends
                        if backend_match_reqs(self.backends[be], reqs)
                    ]
                    beam_template = tmpl
                elif line.startswith("!"):
                    nm, syntax = line[1:].split()[0:2]
                    beam_templates.append((nm, syntax, syntax))
                else:
                    query_mode = "consume"
                    query_mode = do_consume(line)
            elif query_mode == "consume":
                query_mode = do_consume(line)
            else:
                if line.startswith("!beam-query"):
                    query_mode = "start"
                    cur_query = []
                    beam_templates = []
                else:
                    output.append(line)

        return output


class BeamQueryExtension(Extension):
    config = {
        "template_dir": [".", "Directory for template files"],
        "cache_dir": ["./.beam-query-cache", "Directory for cached results"],
        "conf": ["./beam-docs.yaml", "Config file for documentation generation"],
        "enabled_backends": [[], "List of enabled backends (empty list means use all)"],
        "base_dir": [".", "Path to beam source repo"],
    }

    def extendMarkdown(self, md):
        with open(self.getConfig("conf")) as f:
            conf = yaml.load(f, Loader=yaml.SafeLoader)

        backends = conf["backends"]
        is_ci = check_ci()
        enabled_backends = (
            self.getConfig("enabled_backends")
            if not is_ci
            else os.environ["BEAM_DOC_BACKEND"].split()
        )
        print("Enabled backends are", enabled_backends)
        if enabled_backends:
            backends = {k: v for k, v in backends.items() if k in enabled_backends}

        md.preprocessors.register(
            BeamQueryBlockProcessor(
                md,
                template_dir=os.path.abspath(self.getConfig("template_dir")),
                cache_dir=os.path.abspath(self.getConfig("cache_dir")),
                backends=backends,
                base_dir=os.path.abspath(self.getConfig("base_dir")),
            ),
            "beam-query",
            100,
        )


def makeExtension(*args, **kwargs):
    return BeamQueryExtension(*args, **kwargs)