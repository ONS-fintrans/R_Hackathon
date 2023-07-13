# R Hackathon`

## Wordle Game

This repository contains the application and requirements to play Wordle.

Wordle is a game were players have six attempts to guess a five-letter word.
After each guess the tiles will change colour, this indicated to the player if the letters they guessed are in the word / in the correct position.

![image](https://github.com/ONS-fintrans/R_Hackathon/assets/49981451/bfd76f66-6d5e-4e7d-8271-e632dceb399b)


## Directory overview
- www folder contains the images and .css files used within the application
- .gitignore default blocks popular data export file types due to data sensitivity
- Use .env for consistent file paths
- .pre_commit_config.yaml contains pre-commit hooks

## Getting started

```{warning}
Where this documentation refers to the root folder we mean where this README.md is
located.
```

### Requirements

- Access to R

## Set-up

1) Clone this repository

2) Install the R requirements, open your terminal and enter:

```shell
pip install -r requirements.txt
```

3) Auto-setup pre-commit hooks and other features for development, open your terminal and enter:

```shell
make requirements
```

4) (Optional) Set-up .secrets and make template adjustments if required such as .gitignore, additional pre-commit hooks, additional directories.

5) Basic set-up complete. Once project documentation is complete you can create a site view of your documentaion by opening your terminal and entering:

```shell
make docs
```
  For code coverage open your terminal and run:

```shell
make coverage_html
```

## Loading environment variables

To load environment variables such as file paths from the `.env` hidden file you will need to use `dotenv` and `os` if using Python on windows.

1) Open your notebook and import modules
```python
from dotenv import load_dotenv
import os
```
2) Load `.env` file
```python
load_dotenv(override=True)
%env #returns all environment variables
```
3) Select the environment variable you require
```python
path_to_outputs = os.getenv("DIR_OUTPUTS")
print(path_to_outputs) #returns ./outputs
```

## Hidden Files & .Gitignore (if required)

For developers note that if using JuypterLabs that some files such as .gitignore are hidden by default.This limits the possibility to commit privileged data to Git through various filetypes but this is not a replacement for best practice.

3) Select the environment variable you require
```python
my_secret = os.getenv("SECRET_1")
print(my_secret) #returns "yuki is a cute kitten"
```
