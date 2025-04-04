function mkcd -d "Create a directory and set CWD"
  command mkdir $argv
  if [ $status = 0 ]
    switch $argv[(count $argv)]
      case '-*'

      case '*'
        cd $argv[(count $argv)]
        return
    end
  end
end

