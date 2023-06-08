# court_dissents
This is the repository for a collaborative article on why do Czech Constitutional Court judges dissent. The repository builds on the apex court dataset. 

The first major goal is to be able to automatically structure court decisions. To do that, word2vec model is trained on the whole corpus. A sample of decisions is manually annotated on a paragraph level and the paragraphs are then converted to doc2vec representation.

Lastly, multiple classification models are trained and their performance measured in the tidy syntax.\

You can find all the data on my [Google Drive](https://drive.google.com/drive/u/0/folders/1JRuu0mJyU1NvKX_4H4x3gpQC8JggnNfg). The data are updated on a weekly basis on Sunday evening.
