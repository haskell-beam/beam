current_dir=$PWD
for pkg in ./beam-*; do
    cd $pkg
    echo "Checking $pkg"
    cabal check
    cd $current_dir
done