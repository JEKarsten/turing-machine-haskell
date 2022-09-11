sh clean.sh

cd src
echo "Compiling Haskell files ..."
ghc trace-file-generation.hs
ghc trace-log-file-generation.hs
ghc dot-file-generation.hs
echo "Generating trace files ..."
./trace-file-generation
echo "Generating trace log files ..."
./trace-log-file-generation
echo "Generating dot files ..."
./dot-file-generation
echo "Cleaning up ..."
rm *.o
rm *.hi
rm trace-file-generation
rm trace-log-file-generation
rm dot-file-generation
cd ..

cd testing
echo "Converting .dot files to .pdf files ..."
for i in machine*-dotfile.dot; do
    echo "    ${i%-dotfile.dot}"
    dot -Tpdf "$i" -o "${i%-dotfile.dot}-dotfile.pdf" -q
done
cd ..

echo "Done!"