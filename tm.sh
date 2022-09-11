cd src
echo "Compiling tm.hs ..."
ghc tm.hs
rm *.o
rm *.hi
echo "Running $1 ..."
./tm $1
cd ..

echo "Done!"