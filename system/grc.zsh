# GRC colorizes nifty unix tools all over the place
if $(grc &>/dev/null)
then
  if [[ $(uname) == "Darwin" ]]
  then 
    source `brew --prefix`/etc/grc.bashrc
  else
    source /usr/local/etc/grc.bashrc
  fi
fi