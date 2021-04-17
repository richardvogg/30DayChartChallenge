# -*- coding: utf-8 -*-
"""
Created on Sat Sep  5 11:05:24 2020

@author: Richard
"""



def change_encoding(text):
    return text.encode('Windows-1252', errors = "ignore").decode('UTF-8', errors = "ignore")

