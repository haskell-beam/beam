#!/usr/bin/env python

"""
Fenced Code Tabs Extension for Python Markdown
=========================================

This extension generates Bootstrap HTML Tabs for consecutive fenced code blocks

See <https://github.com/yacir/markdown-fenced-code-tabs> for documentation.

Copyright (c) 2017 [Yassir Barchi](https://github.com/yacir).

License: [MIT](https://opensource.org/licenses/MIT)
"""

from __future__ import absolute_import
from __future__ import unicode_literals

from markdown.extensions import Extension
from markdown.preprocessors import Preprocessor

from markdown.extensions.codehilite import CodeHilite, CodeHiliteExtension
from markdown.extensions.codehilite import parse_hl_lines

from collections import deque
from collections import OrderedDict as odict

from time import time

import re
import random
import string

# Ordered so that 'data-' attrs of html are in deterministic order.
PARAM_REGEXES = odict((
    ('hl_lines', re.compile(r'''hl_lines=(?P<quot>"|')(?P<hl_lines>.*?)(?P=quot)''')),
))


# Fenced Code Nav Extension
class FencedCodeTabsPreprocessor(Preprocessor):

    FENCE_BLOCK_REGEX = re.compile(r'''
        (?P<fence>^(?:~{3,}|`{3,}))[ ]*         # Opening ``` or ~~~
        (\{?\.?(?P<lang>[\w#.+-]*))?[ ]*        # Optional {, and lang
        (name=(?P<name>[\w]*))?[ ]*              # Optional name
        # Optional highlight lines, single- or double-quote-delimited
        (hl_lines=(?P<quot>"|')(?P<hl_lines>.*?)(?P=quot))?[ ]*
        }?[ ]*\n                                # Optional closing }
        (?P<code>.*?)(?<=\n)
        (?P=fence)[ ]*$''', re.MULTILINE | re.DOTALL | re.VERBOSE)

    TAB_ITEM_PLACE_HOLDER_TEMPLATE = '<!-- {0}__code_tab__{{0}} -->'
    TAB_ITEM_PLACE_HOLDER_REGEX_TEMPLATE = '<!-- {0}__code_tab__([0-9]+) -->'
    TAB_ITEM_BODY_WRAP_ESCAPE = '<pre><code  class="%s">%s</code></pre>'

    def __init__(self, md, code_fence_tabs_config=None):
        # Initialize the Preprocessor
        self.tab_items = deque()

        self.code_fence_tabs_config = code_fence_tabs_config

        self.checked_for_codehilite = False
        self.codehilite_conf = {}

        placeholder_dict = self._generate_tab_item_placeholder()
        self.tab_item_placeholder = placeholder_dict['placeholder']
        self.tab_item_placeholder_regex = placeholder_dict['placeholder_regex']

        super(FencedCodeTabsPreprocessor, self).__init__(md)

    def _generate_tab_item_placeholder(self):

        # used to create a unique signature
        current_time = time()

        return {
            'placeholder': self.TAB_ITEM_PLACE_HOLDER_TEMPLATE.format(current_time),
            'placeholder_regex': re.compile(self.TAB_ITEM_PLACE_HOLDER_REGEX_TEMPLATE.format(current_time))
        }

    @staticmethod
    def _filter_content(content):

        string_block = content.replace(u'\u2018', '&lsquo;')
        string_block = string_block.replace(u'\u2019', '&rsquo;')
        string_block = string_block.replace(u'\u201c', '&ldquo;')
        string_block = string_block.replace(u'\u201d', '&rdquo;')
        string_block = string_block.replace(u'\u2013', '&ndash;')
        string_block = string_block.replace(u'\xa0', '')

        try:
            string_block = string_block.decode('ascii', 'remove')
        except:
            string_block = content

        return string_block

    @staticmethod
    def _escape(txt):
        # HTML-entity-ize common characters
        txt = txt.replace('&', '&amp;')
        txt = txt.replace('<', '&lt;')
        txt = txt.replace('>', '&gt;')
        txt = txt.replace('"', '&quot;')
        return txt

    def _identify_code_tabs(self, block_str):

        text = FencedCodeTabsPreprocessor._filter_content(block_str)

        while True:
            m = self.FENCE_BLOCK_REGEX.search(text)
            if m:

                first_line = text[m.start():].split('\n')[0]

                kwargs = {}
                for param, regex in PARAM_REGEXES.items():
                    param_m = regex.search(first_line)
                    if param_m:
                        if param_m.group(param):
                            kwargs[param] = param_m.group(param)
                        elif (not param_m.group(param) is None) and param in PARAM_DEFAULTS:
                            kwargs[param] = PARAM_DEFAULTS[param]
                        else:
                            raise Exception("{} needs an argument within \n{}".format(param, first_line))

                lang = ''

                if m.group('lang') and m.group('lang') not in PARAM_REGEXES:
                    lang = m.group('lang')

                name = lang
                if m.group('name'):
                    name = m.group('name')

                if self.codehilite_conf:
                    highliter = CodeHilite(
                        m.group('code'),
                        linenums=self.codehilite_conf['linenums'][0],
                        guess_lang=self.codehilite_conf['guess_lang'][0],
                        css_class=self.codehilite_conf['css_class'][0],
                        style=self.codehilite_conf['pygments_style'][0],
                        lang=(m.group('lang') or None),
                        noclasses=self.codehilite_conf['noclasses'][0],
                        hl_lines=parse_hl_lines(kwargs.get('hl_lines'))
                    )

                    code = highliter.hilite()
                else:
                    code = self.TAB_ITEM_BODY_WRAP_ESCAPE % (lang,
                                                             self._escape(m.group('code')))

                self.tab_items.append(FencedCodeTabs(name.title(), lang, code))

                placeholder = self.tab_item_placeholder.format(len(self.tab_items) - 1)

                text = "{}\n{}\n{}".format(text[:m.start()],
                                           placeholder,
                                           text[m.end():])

            else:
                break

        return text

    def _populate_tabs(self, text):

        lines = text.split('\n')
        start_tab_index = None
        tab_run_length = 0
        tab_set_count = 0
        transformed_lines = ''
        num_tabs = len(self.tab_items)
        single_block_as_tab = self.code_fence_tabs_config['single_block_as_tab']

        for line in lines:
            m = self.tab_item_placeholder_regex.search(line)
            if m:
                if start_tab_index is None:
                    start_tab_index = m.group(1)

                tab_run_length += 1

                # Ignore the remainder of the loop
                continue
            else:

                # We have a non tab save to the tab set so let's aggregate
                # the tabs into a tab set and generate the corresponding HTML
                if len(line.strip()) != 0 and start_tab_index is not None:

                    if single_block_as_tab or tab_run_length > 1:
                        tab_set = FencedCodeTabsSet('tab-' + str(tab_set_count) + '-')
                        tab_set_count += 1

                        for i in range(0, tab_run_length):
                            tab = self.tab_items.popleft()
                            tab_set.add_code_tab(tab)

                        # Convert our tab set (and tabs) into the appropriate HTML
                        tab_html = str(tab_set)
                    else:
                        # Convert our single tab into the appropriate HTML
                        tab_html = str(self.tab_items.popleft())

                    start_tab_index = None
                    tab_run_length = 0
                    transformed_lines += '\n' + self.markdown.htmlStash.store(tab_html) + '\n\n'

                transformed_lines += line + '\n'

        # If there are any remaining tabs enclose them in a final last tab set
        if len(self.tab_items) > 0:
            tab_html = str(self.tab_items[0])

            if single_block_as_tab or len(self.tab_items) > 1:
                tab_set = FencedCodeTabsSet('tab-' + str(num_tabs) + '-')

                for tab in self.tab_items:
                    tab_set.add_code_tab(tab)

                tab_html = str(tab_set)

            transformed_lines += '\n\n' + self.markdown.htmlStash.store(str(tab_html)) + '\n\n'
            self.tab_items.clear()

        return transformed_lines

    def run(self, lines):
        # Check for code hilite extension
        if not self.checked_for_codehilite:
            for ext in self.markdown.registeredExtensions:
                if isinstance(ext, CodeHiliteExtension):
                    self.codehilite_conf = ext.config
                    break

            self.checked_for_codehilite = True

        text = self._identify_code_tabs('\n'.join(lines))
        return self._populate_tabs(text).split('\n')


# Fenced Code Nav
class FencedCodeTabsSet(object):

    TAB_SET_HANDLE_CONTAINER_TEMPLATE = u"""
        <ul class="nav nav-tabs">
            {tabHandles}
        </ul>
    """
    TAB_SET_HANDLE_TEMPLATE = u"""
        <li class="nav-item">
            <a href="#{id}" aria-controls="{id}" role="tab" data-toggle="tab" data-lang="{lang}">{ulang}</a>
        </li>
    """
    TAB_SET_TAB_CONTAINER_TEMPLATE = u"""
        <div class="tab-content">
            {tabs}
        </div>
    """
    TAB_BODY_CONTAINER_TEMPLATE = u"""
        <div role="tabpanel" class="tab-pane {isTabActiveClass}" id="{id}">
            {tabContent}
        </div>
    """
    RANDOM_ID_CHAR_LENGTH = 15

    def __init__(self, tab_id):
        self.id = tab_id
        self.codeTabs = deque()

    def add_code_tab(self, code_tab):
        self.codeTabs.append(code_tab)

    def get_code_tabs(self):
        return self.codeTabs

    def _get_tab_id(self, tab):

        tab_name = tab.get_name()
        tab_id = tab_name

        if tab_name is None or not tab_name.strip():
            tab_id = ''. join(
                random.SystemRandom().choice(
                    string.ascii_lowercase + string.digits
                ) for _ in range(self.RANDOM_ID_CHAR_LENGTH)
            )

        tab_set_id = self.id + tab_id

        return tab_set_id

    def __str__(self):
        tab_active_class = 'active'
        tab_handles = ''
        tabs = ''

        for tab in self.codeTabs:

            tab_set_id = self._get_tab_id(tab)
            lang = tab.get_lang()

            tab_handles += self.TAB_SET_HANDLE_TEMPLATE.format(id=tab_set_id,
                                                               isTabActiveClass=tab_active_class,
                                                               lang=lang,
                                                               ulang=tab.get_name())

            tabs += self.TAB_BODY_CONTAINER_TEMPLATE.format(id=tab_set_id,
                                                            isTabActiveClass=tab_active_class,
                                                            lang=tab.get_lang(),
                                                            tabContent=tab)
            tab_active_class = ''

        tab_set_str = self.TAB_SET_HANDLE_CONTAINER_TEMPLATE.format(tabHandles=tab_handles)
        tab_set_str += self.TAB_SET_TAB_CONTAINER_TEMPLATE.format(tabs=tabs)

        return """
                <div class="code-nav-container">
                    {tabSet}
                </div>
                    """.format(tabSet=tab_set_str)

    def __repr__(self):
        return self.__str__()


# Fenced Code Nav Item
class FencedCodeTabs(object):

    def __init__(self, name, lang, body):
        self.name = name
        self.lang = lang
        self.tabBody = body

    def __str__(self):
        return self.tabBody

    def get_lang(self):
        return self.lang

    def get_name(self):
        return self.name

    def __repr__(self):
        return self.__str__()


# Extension Class
class FencedCodeTabsExtension(Extension):

    def __init__(self, *args, **kwargs):

        # Config defaults
        self.config = {
            'single_block_as_tab': [False, 'Render a single ``` code block as a tab'],
        }

        super(FencedCodeTabsExtension, self).__init__(*args, **kwargs)

    @staticmethod
    def to_bool(param):
        the_bool = param

        if isinstance(param, str):
            the_bool = False if param.lower() is 'false' else True

        return the_bool

    def extendMarkdown(self, md, md_globals):

        # Just in case convert this parameter to a bool if it is sent in as a string
        self.setConfig('single_block_as_tab', FencedCodeTabsExtension.to_bool(
            self.getConfig('single_block_as_tab')
        ))

        md.registerExtension(self)

        # Add FencedCodeTabsPreprocessor to the Markdown instance.
        md.preprocessors.add('fenced_code_block',
                             FencedCodeTabsPreprocessor(md, self.getConfigs()),
                             '>normalize_whitespace')


def makeExtension(*args, **kwargs):
    return FencedCodeTabsExtension(*args, **kwargs)
