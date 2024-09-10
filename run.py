import os
import sys
import tempfile

# Read the snippet name from the command line
snippet_name = sys.argv[1]

# Search the markdown files for the snippet name
markdown_files = [f for f in os.listdir('.') if f.endswith('.md')]

target_block_content = None

for markdown_file in markdown_files:
    with open(markdown_file, 'r') as f:
        # If there is a line containing "NAME={snippet_name}",
        # and it is inside a triple-backticks block,
        # then extract the block's content
        lines = f.readlines()
        is_codeblock = False
        current_block_contents = []
        for i, line in enumerate(lines):
            if '```' in line:
                is_codeblock = not is_codeblock
                if not is_codeblock:
                    # code block ended, check for name tag
                    for j, block_line in enumerate(current_block_contents):
                        if f'NAME={snippet_name}' in block_line:
                            target_block_content = current_block_contents
                            break
                    
                    # reset block contents
                    current_block_contents = []
            if is_codeblock:
                current_block_contents.append(line)

if not target_block_content:
    print(f'No code block found for snippet {snippet_name}')
    sys.exit(1)

# If the code block was found: look for cmdline
# (first line is extra, so delete it)
target_block_content = target_block_content[1:]
cmdline = None
for line in target_block_content:
    if 'CMDLINE=' in line:
        cmdline = line.split('=')[1].strip()

if not cmdline:
    print(f'No cmdline found for snippet {snippet_name}')
    sys.exit(1)

# Save the snippet to a temporary file
with tempfile.NamedTemporaryFile(suffix=snippet_name, mode='w', delete=False) as f:
    f.write('\n'.join(target_block_content))
    f.flush()

# Run the snippet
os.system(cmdline.replace("$FILE", f.name))

# Delete the temporary file
os.remove(f.name)