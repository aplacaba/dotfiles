# WSL2 - Launch Emacs

# Get the IP to use from wsl and set to a variable.
$wslip = wsl zsh -c 'ip route | awk ''/default via /'' | cut -d'' '' -f3'

# Run Emacs
wsl zsh -c "export DISPLAY=$wslip`:0.0 export LIBGL_ALWAYS_INDIRECT=1 && setsid emacs"