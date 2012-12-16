"""
Homework 4 Module

Classes for authorship recognition

Matt Adelman
"""

import math
import string

class FunctionWordModel(object):
    """
    A FunctionWordModel instance represents a function-word model of a 
    particular author. Such a model intuitively consists of a map m with 
    function-words as keys so that m[w] is P[{w} | F] where {w} is the event
    of choosing w from the words in the texts and F is the event of choosing
    a function-word from the words in the texts. The function words are 
    specified when the instance is created and cannot be modified after 
    that. Training texts may be added at any time.

    For the purposes of identifying function words in a text, a "word" in 
    any text is defined to be a contiguous sequence of non-whitespace 
    characters delimited by whitespace (including the beginning and end of 
    the text).    
    """

    def __init__(self, function_words):
        """
        Initializes a FunctionWord instance.

        Paramters:
            function_words (collection of string) - the collection of 
            function words to use for this model. function_words must 
            support the iterator interface. Duplicates in function_words 
            will be discarded.
        """
        self.function_words = function_words # function words
        self.model = {} # map for model
        self.fn_word_occurrences = {} # of occurrences of w added to model
        self.total_fn_words = 0.0 # number of function words total

    def add_text(self, text):
        """
        Add a training text to this model.

        Paramteters:
            text (string) - a string representing the training text to add 
            to this model.
        Returns: NoneType
            None    
        """
        word_list = []
        # creates a list of all the words in the text

        word_list = text.split()

        # examines each word to see if it is a function words    
        for word in word_list:

            # counts the number of occurrences of each funtion word
            # and the total number of function words
            if word not in self.function_words:
                continue
            else:
                if word not in self.fn_word_occurrences:
                    self.fn_word_occurrences[word] = 1
                    self.total_fn_words +=1
                else:
                    self.fn_word_occurrences[word] +=1
                    self.total_fn_words +=1

        # create the model
        for word in self.fn_word_occurrences:

            self.model[word] = (self.fn_word_occurrences[word]) / \
                               (self.total_fn_words)  
        
        return 

    def log_likelihood(self, text):
        """
        Compute the log-likelihood that a given text was written by the 
        author represented by this model.

        Parameters:
            text (string) - a string representing the text to score against 
            this model.
        Returns: float
            The log-likelihood that the author reprsented by this model 
            wrote text. The log-likelihood is computed using naive smoothing
            for words that appear in text but not model.        
        """
        unknown_word_list = []
        unknown_occurrences = {}
        log_likelihood = 0.0

        # collecting data on the unknown text
        # basically the same as add_text
        unknown_word_list = text.split()

        for word in unknown_word_list:
            
            if word not in self.function_words:
                continue
            else:
                if word not in unknown_occurrences:
                    unknown_occurrences[word] = 1
                else:
                    unknown_occurrences[word] +=1
        
        # calculating the log likelihood
        for word in unknown_occurrences:
            # if word is in the original model
            if word in self.model:
                log_likelihood = log_likelihood + (unknown_occurrences[word]
                                 * math.log(self.model[word]))
            # using naive smoothing    
            else:
                log_likelihood = log_likelihood + (unknown_occurrences[word]
                                 * math.log(1 / self.total_fn_words))
        
        return (log_likelihood * -1)

class SentenceLengthModel(object):
    """
    An instance of this class represents the sentence-length model based on
    given training texts. A setence-terminator is a period, question-mark, 
    or exclamation point. A sentence is any string of characters delimited 
    by sentence-terminators. A word is any sequence of non-whitespace 
    characters delimited by whitespace. The length of a sentence is the 
    number of words in the sentence. The sentence-length model intuitively 
    consists of a map from each possible sentence length L to the 
    probability that a randomly-chosen sentence from the training texts has
    length L.
    """
    def __init__(self):
        """
        Create a SentenceLengthModel instance.
        """
        self.model = {} # map for model
        self.total_num_sentences = 0.0 # total sentence count in 
                                       # training texts
        self.sl_occurrences = {} # map of the number of times a given 
                                # sentence occurs
        

    def add_text(self, text):
        """
        Add a training text to this model.

        Parameters:
            text (string) - the text to add to this model.
        Returns: NoneType
            None        
        """
        # The string variables are used when doctoring the given text, i.e.
        # chaning the sentence terminators, or removing newline characters.
        # The first list is for keeping track of every sentence, and the 
        # second is to keep track of the lengths of the sentences.
        string4 = " "
        string_length_list = []
        sentence_length_list = []

        # This block doctors the text to make it more compatible with our
        # definition of a sentence
        string2 = text.replace("!", ".")
        string3 = string2.replace("?", ".")       
        for w in string3:
            ww = w.strip("\n")
            string4 = string4 + ww

        # Creates the list of all the sentences as seperate strings.
        string_length_list = string4.split(".")
        
        # Removing the last element of the list (for some reason the split
        # function always tacks on an empty string at the end)
        del string_length_list[-1]

        # Getting the length of every sentence
        for string in string_length_list:
            x = string.split()
            sentence_length_list.append(len(x))

        # Storing the total number of sentences (for naive smooting)
        self.total_num_sentences = self.total_num_sentences + \
                                   len(sentence_length_list)

        # Creating a map of the number of times a given sentence length
        # occurs in a training text
        for length in sentence_length_list:
            if length not in self.sl_occurrences:
                self.sl_occurrences[length] = 1
            else:
                self.sl_occurrences[length] +=1

        # Creating the probability model for the given training text 
        for sl in self.sl_occurrences:
            self.model[sl] = self.sl_occurrences[sl] / \
                               self.total_num_sentences

        return

    def log_likelihood(self, text):
        """
        Compute the log-likelihood that a given text was written by the 
        author represented by this model.

        Parameters:
            text (string) - the text to score against this model.
        Returns: float
            The log-likelihood that the author reprsented by this model 
            wrote text. The log-likelihood is computed using naive smoothing
            for sentence lengths that appear in text but not this model.
        """
        # Until noted, this serves basically the same function as above,
        # but just to get the sentence length occurrences for the unknown
        # text.
        unknown_sl_list = []
        unknown_string_list = []
        unknown_occurrences = {}
        log_likelihood = 0.0

        string4 = " "
        string2 = text.replace("!", ".")
        string3 = string2.replace("?", ".")
        
        for w in string3:
            ww = w.strip("\n")
            string4 = string4 + ww

        unknown_string_list = string4.split(".")
        
        del unknown_string_list[-1]

        for string in unknown_string_list:
            x = string.split()
            unknown_sl_list.append(len(x))

        for length in unknown_sl_list:
            if length not in unknown_occurrences:
                unknown_occurrences[length] = 1
            else:
                unknown_occurrences[length] +=1

        # Calculating the log-likelihood
        for sl in unknown_occurrences:
            if sl in self.model:
                log_likelihood = log_likelihood + (unknown_occurrences[sl] *
                                 math.log(self.model[sl]))
            # Naive smoothing (if necessary)
            else:
                log_likelihood = log_likelihood + (unknown_occurrences[sl] *
                                 math.log((1 / self.total_num_sentences)))

        return log_likelihood * -1  
