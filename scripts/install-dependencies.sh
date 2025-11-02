wget https://storage.googleapis.com/dart-archive/channels/stable/release/2.19.6/linux_packages/dart_2.19.6-1_amd64.deb
sudo dpkg -i dart_2.19.6-1_amd64.deb
rm dart_2.19.6-1_amd64.deb
export PATH="$PATH:/usr/lib/dart/bin"
wget https://download.racket-lang.org/installers/8.18/racket-8.18-x86_64-linux-buster-cs.sh
chmod +x racket-8.18-x86_64-linux-buster-cs.sh
sudo ./racket-8.18-x86_64-linux-buster-cs.sh
rm racket-8.18-x86_64-linux-buster-cs.sh
export PATH="$PATH:/usr/racket/bin"