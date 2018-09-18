function nvm
    bass source ~/.nvm/nvm.sh --no-use ';' nvm $argv
end

function xcopy
  xclip -sel clip
end

function tcopy
  tmux show buffer '|' xclip -sel clip
end
