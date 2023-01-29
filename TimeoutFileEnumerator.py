import os

def find_files_with_timeout(directory):
    text_of_files = {}
    keywords = ["Sketch Not Resolved Error", "Assertion failed", "ERROR", "timeout"]
    keyword_sketches = set()
    ret = {x : [] for x in keywords}
    no_keyword_files = []
    sketches = []
    for root, dirs, files in os.walk(directory):
        for file in files:
            file_path = os.path.join(root, file)
            with open(file_path, 'r') as f:
                file_contents = f.read()
                text_of_files[file_path] = file_contents
                # if "miniTest207MainMethodAndPragma" in file_path:
                #     print(file_contents)
                #     print("-------------------------")
                #     print("-------------------------")
                #     print("-------------------------")
                #     print("-------------------------")
                enter = False
                for keyword in keywords:
                    if keyword in file_contents:
                        ret[keyword].append(file_path)
                        enter = True
                if enter:
                    keyword_sketches.add((file_path.split("/")[-1]).split(".")[0]+".sk")
                if not enter and file_path[-7:] == ".output":
                    no_keyword_files.append(file_path)
                if file_path[-3:] == ".sk":
                    assert not enter
                    sketches.append(file_path)
    return ret, no_keyword_files, text_of_files, sketches, keyword_sketches

def main():

    ret, nonto_files, text_of_files, sketches, keyword_sketches = find_files_with_timeout("/Users/klimentserafimov/CLionProjects/sketch-frontend/src/test/sk/seq")

    for keyword in ret:
        print(keyword, len(ret[keyword]))
        # for row in ret[keyword]:
        #     print(row)

    print("nonto_files", len(nonto_files))
    # for row in nonto_files:
    #     print(row)

    print()
    print("Unaccounted ERROR files:")
    for row in ret["ERROR"]:
        output_text = text_of_files[row]
        unaccounted_error = "Assertion failed" not in output_text and "timeout" not in output_text
        sk_name = row[:-7]+".sk"
        sk_text = text_of_files[sk_name]
        rez = "minimize" in sk_text or "model" in sk_text
        if unaccounted_error:
            print(row)
            print("Unaccounted")
            if rez:
                print("has minimize or model")
            else:
                print("truly unaccounted")

    print()
    print("Assertion failed sk files wo minimize or model")
    for row in ret["Assertion failed"]:
        sk_name = row[:-7]+".sk"
        sk_text = text_of_files[sk_name]
        rez = "minimize" in sk_text or "model" in sk_text or "hasglobalhole" in sk_text

        if not rez:
            print(row)
            print(row[:-7])
            print(row[:-7]+".sk")
            print(rez)



    print("KEYWORD SKETCHES")
    for row in keyword_sketches:
        print(row)
    print("DONE KEYWORD SKETCHES")

    counterexamples = []

    for row in sketches:
        assert row[-3:] == ".sk"
        has = False
        if "minimize" in text_of_files[row]:
            print(row, "has", "minimize")
            has = True
        if "model" in text_of_files[row]:
            print(row, "has", "model")
            has = True
            # print(text_of_files[row])

        if has:
            sk_name = row.split("/")[-1]
            print(sk_name)
            rez = (sk_name in keyword_sketches)
            if not rez:
                counterexamples.append(sk_name)

    print("counterexamples ", len(counterexamples))
    for row in counterexamples:
        print(row)


main()