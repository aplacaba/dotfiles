set shell = WScript.CreateObject("Shell.Application")
shell.ShellExecute "wsl.exe", "zsh -c -l ""emacs""", "", "open", 0