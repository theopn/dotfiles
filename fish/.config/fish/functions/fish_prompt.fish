function fish_prompt -d "Mumbo's Vim statusline esque Fish prompt"
  # Nordfox
  set -l bg0 232831
  set -l bg2 39404F
  set -l fg2 ABB1BB
  set -l fg3 7E8188
  set -l cyan 88C0D0
  set -l green A3BE8C
  set -l purple B48EAD
  set -l red BF616A
  set -l blue 81A1C1

  set -l normal (set_color normal)

  # From Fish arrow prompt minus HG info
  # faster than (fish_git_prompt)
  if not set -q -g __fish_arrow_functions_defined
    set -g __fish_arrow_functions_defined
    function _git_branch_name
      set -l branch (git symbolic-ref --quiet HEAD 2>/dev/null)
      if set -q branch[1]
        echo (string replace -r '^refs/heads/' '' $branch)
      else
        echo (git rev-parse --short HEAD 2>/dev/null)
      end
    end

    function _is_git_dirty
      not command git diff-index --cached --quiet HEAD -- &>/dev/null
      or not command git diff --no-ext-diff --quiet --exit-code &>/dev/null
    end

    function _is_git_repo
      type -q git
      or return 1
      git rev-parse --git-dir >/dev/null 2>&1
    end
  end

  # CWD
  # no need for basename() prompt_pwd does truncation by default
  set -l cwd (set_color --bold $fg2 --background $bg0) (prompt_pwd)

  # Prompt
  set -l prompt (set_color $fg3) '$'
  if fish_is_root_user
    set prompt (set_color --bold $red) '#'
  end

  # Git info
  # Hard code 
  set -l BIG_REPOS Robot-Cello-ResidualRL Robot-Cello
  set -l git_info
  if not contains (basename $PWD) $BIG_REPOS
    if _is_git_repo
      set git_info (set_color --bold $fg3 --background $bg2) (_git_branch_name)

      if _is_git_dirty
        set git_info "$git_info ! "
      else
        set git_info "$git_info "
      end
    end
  end

  echo -n -s "$cwd $git_info" $normal ' ' $prompt $normal ' '
end

