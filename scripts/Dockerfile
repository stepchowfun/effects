FROM debian:stretch

# We install the `libtinfo-dev` package to work around a strange bug in Stack.
# Without it, we get this error when running `stack install brittany` below:
#   <command line>: can't load .so/.DLL for: libtinfo.so
#   (libtinfo.so: cannot open shared object file: No such file or directory)
RUN \
  DEBIAN_FRONTEND=noninteractive apt-get -y update && \
  DEBIAN_FRONTEND=noninteractive apt-get -y install \
    'build-essential=12.3' \
    'libtinfo-dev=6.0+*' \
    'ruby=1:2.3.*' && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /usr/share/doc

# The Stack installation script apparently uses apt-get.
RUN \
  DEBIAN_FRONTEND=noninteractive apt-get -y update && \
  DEBIAN_FRONTEND=noninteractive apt-get -y install 'curl=7.52.*' && \
  curl -sSL https://get.haskellstack.org/ | sh && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /usr/share/doc && \
  DEBIAN_FRONTEND=noninteractive apt-get -y purge --auto-remove curl

RUN useradd --user-group --create-home user

USER user:user

WORKDIR /home/user

# Stack installs executables in $HOME/.local/bin.
RUN \
  mkdir -p "$HOME/.local/bin" && \
  echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.profile

RUN stack setup --resolver lts-10.4

# The `mkdir` command is needed to work around a bug in brittany. See
# https://github.com/lspitzner/brittany/issues/115 for details.
RUN mkdir "$HOME/.config" && stack install brittany
