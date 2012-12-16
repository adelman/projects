"""
Module ar

A program for authorship recognition.

Matt Adelman
"""

import sys
import hw4b
import pickle
import string

def main(argv):
    """
    Author recognition program. The program is invoked as follows:

        python ar.py build --f fn_words output_file text1 text2...
        python ar.py build --s output_file text1 text2...
        python ar.py score text model1 model2...

    The first two invocations create a function-word (--f) or 
    sentence-length (--s) model for an author (note: there are two dashes 
    for these options). fn_words is the name of a file with the function 
    words to use for the model; it must consist of one word per line with 
    no blank lines. output_file is the name of the file to which to write 
    the model. text1, text2,... are file names of texts written by the 
    author. output_file consists of a pickled hw4.FunctionWordModel or 
    hw4.SentenceLengthModel.

    The third invocation scores a text against a collection of models. text
    is the text to score and model1, model2,... are models created by 
    invoking ar.py with the build command. Of course, they should all be the
    same type of model (i.e., all function-word models with the same list of
    function words or all sentence-length models). The texts are scored by 
    invoking the log_likelihood method of each model.        
    """
    # This is for the invocation where a model is built
    if argv[1] == "build":

        # This is the function word model
        if argv[2] == "--f":

            # Naming the function word file, and the output file
            fn_filename = argv[3]
            out_filename = argv[4]

            # A set to hold the function words
            fn_set = set()

            # Opening the function words, stripping the newline characters
            # and adding them to the set
            fn_words = open(fn_filename, 'r')
            for w in fn_words:
                ww = w.strip("\n")
                fn_set.add(ww)
            fn_words.close()

            # Creating a function word model for the given author
            fw_model = hw4b.FunctionWordModel(fn_set)

            # Looks through the given texts and adds them to the model
            # but first converts the files to strings
            for text in argv[5:]:
                string = " " # becomes text to send to model
                text_x = open(text, 'r')
                for line in text_x:
                    string  = line + string
                fw_model.add_text(string)
                text_x.close()
                
            
            # Opens the output file
            outfile = open(out_filename, 'wb')

            # Pickles the output file
            pickle.dump(fw_model, outfile)

            outfile.close()

        # The invocation to create a sentence lenght model
        else:

            # Naming the output file
            out_filename = argv[3]

            # Creates a sentence length model
            sl_model = hw4b.SentenceLengthModel()

            # Looks through the given texts and adds them to the model
            # but first converts them to strings
            for text in argv[4:]:
                string = " "# becomes text to send to model
                text_x = open(text, 'r')
                for line in text_x:
                    string = string + line
                sl_model.add_text(string)
                text_x.close()

            # Opens the output file
            outfile = open(out_filename, 'wb')
            
            # Pickles the output file
            pickle.dump(sl_model, outfile)

            outfile.close()
    
    # Invocation to score texts
    else:
        
        # Naming and opening the unknown text file
        unknown_text = argv[2]
        text = open(unknown_text, 'r')

        # A list of scores to print out from the models
        score_list = []

        string = " "# becomes text to send to model
        for line in text:
            string = string + line


        # Opening the models, unpickling them, and sending them to the log-
        # likelihood method. The score is then appended to the score list
        for models in argv[3:]:
            model_x = open(models, 'rb')
            score_model = pickle.load(model_x)
            score_list.append(score_model.log_likelihood(string))
        
        # Printing the score list
        for i in range(1, len(argv[3:]) + 1, 1):
            print "The score for model %d is %.08f" % (i, score_list[i-1])


if __name__ == '__main__':
    main(sys.argv)
