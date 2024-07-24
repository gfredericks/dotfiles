import openai
import os
import re
from typing import Iterator
import json
import subprocess
import sys
import shutil


def mkclient():
    config=json.loads(os.getenv("OPENAI_CFG"))
    if config['cls'] == 'AzureOpenAI':
        return openai.AzureOpenAI(
            api_key=os.getenv("OPENAI_API_KEY"),
            api_version=config['api_version'],
            azure_endpoint=config['azure_endpoint'],
        )
    elif config['cls'] == 'OpenAI':
        return openai.OpenAI(
            api_key=os.getenv("OPENAI_API_KEY"),
            organization=config['organization'],
            project=config['project']
        )

WHITESPACE=re.compile('[ \n]')

def extract_whitespace(chunks: Iterator[str]) -> Iterator[str]:
    for s in chunks:
        if s is None:
            continue
        while len(s):
            if m := WHITESPACE.search(s):
                idx=m.span()[0]
                if idx > 0:
                    yield s[0:idx]
                yield s[idx:(idx+1)]
                s = s[(idx+1):]
            else:
                yield s
                break

def reformat(chunks: Iterator[str]) -> Iterator[str]:
    target_width= min(80, shutil.get_terminal_size()[0])
    line_len = 0
    current_stream = ""
    is_first_chunk = True
    just_did_paragraph_space = False

    # this bit would probably be easier if I did a while True loop?
    ansi_after_next_newline = None

    mode = 'paragraph'
    for s in extract_whitespace(chunks):
        if line_len == 0 and s == '```':
            if mode == 'paragraph':
                yield s
                ansi_after_next_newline = '\u001b[31;1m'
                mode = 'code'
            elif mode == 'code':
                yield '\u001b[0m'
                yield s
                mode = 'paragraph'
            else:
                raise RuntimeError("How'd we get here?")
        elif s == '\n':
            line_len = 0
            yield s
            if ansi_after_next_newline:
                yield ansi_after_next_newline
                ansi_after_next_newline = None
        elif mode == 'paragraph' and s == ' ':
            just_did_paragraph_space = True
            line_len += 1
        elif mode == 'paragraph':
            new_len = line_len + len(s)
            if just_did_paragraph_space:
                if new_len > target_width:
                    yield '\n'
                    line_len = len(s)
                else:
                    yield ' '
                    line_len = new_len
                yield s
                just_did_paragraph_space = False
            else:
                yield s
                line_len = new_len
                just_did_paragraph_space = False
        else:
            yield s
    yield '\n'

def main():
    cmd = sys.argv[1]
    if cmd in ("img", "imgurl"):
        prompt = ' '.join(sys.argv[2:])
        resp = mkclient().images.generate(
            model="dall-e-3",
            prompt=prompt,
            n=1,
            size="1024x1024"
        )
        data = resp.data[0]
        url = data.url
        if cmd == "img":
            subprocess.run(["open-url-in-browser-from-remote", url])
        elif cmd == "imgurl":
            print(url)
        else:
            raise Exception("unreachable")
    elif cmd == "chat":
        question = ' '.join(sys.argv[2:])
        stream = mkclient().chat.completions.create(
            # model="gpt-3.5-turbo",
            model="gpt-4o",
            messages=[
                {"role": "system", "content": "You are a helpful assistant who is very succinct unless asked otherwise; if a question for a code example is asked for, you should give just the code with no prose or code comments."},
                {"role": "user", "content": question}
            ],
            stream=True
        )

        for chunk in reformat(chunk.choices[0].delta.content for chunk in stream):
            print(chunk, end="", flush=True)
    else:
        print(f"What is {sys.argv!r}?")
        exit(3)

if __name__ == '__main__':
    main()
