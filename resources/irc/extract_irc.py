import html.parser
import re
import sys

class IRCExtractor(html.parser.HTMLParser):
    def __init__(self):
        super().__init__()
        self.in_section = False
        self.depth = 0
        self.result = []
        self.skip_tags = {'script', 'style', 'nav', 'form', 'input', 'button'}
        self.skipping = 0
        
    def handle_starttag(self, tag, attrs):
        attrs_dict = dict(attrs)
        cls = attrs_dict.get('class', '')
        
        # Start capturing at div.section (the actual statutory content)
        if tag == 'div' and 'section' == cls.strip():
            self.in_section = True
            self.depth = 1
            return
            
        if self.in_section:
            if tag == 'div':
                self.depth += 1
            if tag in self.skip_tags:
                self.skipping += 1
                return
            if tag in ('p', 'div', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'tr', 'li'):
                self.result.append('\n')
            if tag == 'br':
                self.result.append('\n')
                
    def handle_endtag(self, tag):
        if self.in_section:
            if tag == 'div':
                self.depth -= 1
                if self.depth <= 0:
                    self.in_section = False
            if tag in self.skip_tags and self.skipping > 0:
                self.skipping -= 1
            if tag == 'p':
                self.result.append('\n')
                
    def handle_data(self, data):
        if self.in_section and self.skipping == 0:
            self.result.append(data)

html_content = sys.stdin.read()
extractor = IRCExtractor()
extractor.feed(html_content)
text = ''.join(extractor.result)

# Clean up
text = re.sub(r'[ \t]+', ' ', text)
text = re.sub(r'\n ', '\n', text)
text = re.sub(r'\n{3,}', '\n\n', text)
text = text.strip()

print(text)
