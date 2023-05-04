counter=0
max_counter=0
for f in ./examples/good/*; do
    max_counter=$((max_counter+1))

    echo "\"$f\"..."

    ./build/TestRuskell < "$f" > /dev/null

    if [ $? -eq 0 ]; then
        counter=$((counter+1))
        echo "OK"
    else
        echo "ERROR"
    fi
done

echo "Passed $counter/$max_counter tests"
