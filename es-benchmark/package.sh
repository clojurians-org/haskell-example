MY_STORE=$(ls -l result | awk '{print $NF}')
nix-store --export $(nix-store -qR $MY_STORE) | gzip > my.naz.gz
