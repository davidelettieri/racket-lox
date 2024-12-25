apt-get update && apt-get install -y git wget racket
wget https://storage.googleapis.com/dart-archive/channels/stable/release/2.19.6/linux_packages/dart_2.19.6-1_amd64.deb
dpkg -i dart_2.19.6-1_amd64.deb 
raco pkg install racket-langserver