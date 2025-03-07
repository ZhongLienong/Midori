import os
import subprocess

YELLOW = "\033[33m"
RESET = "\033[0m"

script_dir = os.path.dirname(os.path.abspath(__file__))
exe_path = "C:\\Users\\jk381\\source\\repos\\ZhongLienong\\Midori\\out\\build\\x64-release\\Midori.exe"

for root, _, files in os.walk(script_dir):
    for file in files:
        file_path = os.path.join(root, file)
        
        if file_path == os.path.abspath(__file__):
            continue
        
        print(f"{YELLOW}Running: Midori.exe \"{file_path}\"{RESET}")
        subprocess.run([exe_path, file_path], check=False)
        print()