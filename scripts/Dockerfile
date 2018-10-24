FROM debian:stretch

RUN \
  DEBIAN_FRONTEND=noninteractive apt-get -y update && \
  DEBIAN_FRONTEND=noninteractive apt-get -y install \
    'build-essential=12.3' \
    'ruby=1:2.3.3' && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /usr/share/doc

# The Stack installation script apparently uses apt-get.
RUN \
  DEBIAN_FRONTEND=noninteractive apt-get -y update && \
  DEBIAN_FRONTEND=noninteractive apt-get -y install \
    'curl=7.52.1-5+deb9u7' && \
  curl -sSL https://get.haskellstack.org/ | sh && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /usr/share/doc && \
  DEBIAN_FRONTEND=noninteractive apt-get -y purge --auto-remove curl

RUN useradd --user-group --create-home user

USER user:user

WORKDIR /home/user

# Without this, Ruby will assume files are encoded as ASCII.
RUN echo 'export LANG="C.UTF-8"' >> ~/.profile

# Stack installs executables in $HOME/.local/bin.
RUN \
  mkdir -p "$HOME/.local/bin" && \
  echo 'export PATH="$HOME/.local/bin:$PATH"' >> ~/.profile

RUN stack setup --resolver lts-12.14

RUN stack install hindent hlint
