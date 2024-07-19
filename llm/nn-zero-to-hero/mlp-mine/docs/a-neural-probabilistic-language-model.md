# A Neural Probabilistic Language Model

**Yoshua Bengio**  
*BENGIOY@IRO.UMONTREAL.CA*

**Réjean Ducharme**  
*DUCHARME@IRO.UMONTREAL.CA*

**Pascal Vincent**  
*VINCENTP@IRO.UMONTREAL.CA*

**Christian Jauvin**  
*JAUVINC@IRO.UMONTREAL.CA*

Département d’Informatique et Recherche Opérationnelle  
Centre de Recherche Mathématiques  
Université de Montréal, Montréal, Québec, Canada

Editors: Jaz Kandola, Thomas Hofmann, Tomaso Poggio and John Shawe-Taylor

origin pdf: https://www.jmlr.org/papers/volume3/bengio03a/bengio03a.pdf

## Abstract

A goal of statistical language modeling is to learn the joint probability function of sequences of words in a language. This is intrinsically difficult because of the curse of dimensionality: a word sequence on which the model will be tested is likely to be different from all the word sequences seen during training. Traditional but very successful approaches based on n-grams obtain generalization by concatenating very short overlapping sequences seen in the training set. We propose to fight the curse of dimensionality by learning a distributed representation for words which allows each training sentence to inform the model about an exponential number of semantically neighboring sentences. The model learns simultaneously (1) a distributed representation for each word along with (2) the probability function for word sequences, expressed in terms of these representations. Generalization is obtained because a sequence of words that has never been seen before gets high probability if it is made of words that are similar (in the sense of having a nearby representation) to words forming an already seen sentence. Training such large models (with millions of parameters) within a reasonable time is itself a significant challenge. We report on experiments using neural networks for the probability function, showing on two text corpora that the proposed approach significantly improves on state-of-the-art n-gram models, and that the proposed approach allows to take advantage of longer contexts.

*Keywords: Statistical language modeling, artificial neural networks, distributed representation, curse of dimensionality*

## 1. Introduction

A fundamental problem that makes language modeling and other learning problems difficult is the curse of dimensionality. It is particularly obvious in the case when one wants to model the joint distribution between many discrete random variables (such as words in a sentence, or discrete attributes in a data-mining task). For example, if one wants to model the joint distribution of 10 consecutive words in a natural language with a vocabulary $ V $ of size 100,000, there are potentially $ 100,000^{10} - 1 = 10^{50} - 1 $ free parameters. When modeling continuous variables, we obtain generalization more easily (e.g. with smooth classes of functions like multi-layer neural networks or Gaussian mixture models) because the function to be learned can be expected to have some local smoothness properties. For discrete spaces, the generalization structure is not as obvious: any change of these discrete variables may have a drastic impact on the value of the function to be estimated, and when the number of values that each discrete variable can take is large, most observed objects are almost maximally far from each other in hamming distance.

A useful way to visualize how different learning algorithms generalize, inspired from the view of non-parametric density estimation, is to think of how probability mass that is initially concentrated on the training points (e.g., training sentences) is distributed in a larger volume, usually in some form of neighborhood around the training points. In high dimensions, it is crucial to distribute probability mass where it matters rather than uniformly in all directions around each training point. We will show in this paper that the way in which the approach proposed here generalizes is fundamentally different from the way in which previous state-of-the-art statistical language modeling approaches are generalizing.

A statistical model of language can be represented by the conditional probability of the next word given all the previous ones, since

$$  P(w_1^T) = \prod_{t=1}^T P(w_t | w_1^{t-1}),  $$

where $  w_t  $ is the t-th word, and writing sub-sequence $  w_i^j = (w_i, w_{i+1}, \ldots, w_{j-1}, w_j)  $. Such statistical language models have already been found useful in many technological applications involving natural language, such as speech recognition, language translation, and information retrieval. Improvements in statistical language models could thus have a significant impact on such applications.

When building statistical models of natural language, one considerably reduces the difficulty of this modeling problem by taking advantage of word order, and the fact that temporally closer words in the word sequence are statistically more dependent. Thus, n-gram models construct tables of conditional probabilities for the next word, for each one of a large number of contexts, i.e. combinations of the last $ n-1 $ words:

$$  P(w_t | w_1^{t-1}) \approx P(w_t | w_{t-n+1}^{t-1}).  $$

We only consider those combinations of successive words that actually occur in the training corpus, or that occur frequently enough. What happens when a new combination of $ n $ words appears that was not seen in the training corpus? We do not want to assign zero probability to such cases, because such new combinations are likely to occur, and they will occur even more frequently for larger context sizes. A simple answer is to look at the probability predicted using a smaller context size, as done in back-off trigram models (Katz, 1987) or in smoothed (or interpolated) trigram models (Jelinek and Mercer, 1980). So, in such models, how is generalization basically obtained from sequences of words seen in the training corpus to new sequences of words? A way to understand how this happens is to think about a generative model corresponding to these interpolated or back-off n-gram models. Essentially, a new sequence of words is generated by “gluing” very short and overlapping pieces of length 1, 2 ... or up to $ n $ words that have been seen frequently in the training data. The rules for obtaining the probability of the next piece are implicit in the particulars of the back-off or interpolated n-gram algorithm. Typically researchers have used $ n = 3 $, i.e. trigrams, and obtained state-of-the-art results, but see Goodman (2001) for how combining many tricks can yield substantial improvements. Obviously there is much more information in the sequence that immediately precedes the word to predict than just the identity of the previous couple of words. There are at least two characteristics in this approach which beg to be improved upon, and that we will focus on in this paper. First, it is not taking into account contexts farther than 1 or 2 words, second it is not taking into account the “similarity” between words. For example, having seen the sentence “The cat is walking in the bedroom” in the training corpus should help us generalize to make the sentence “A dog was running in a room” almost as likely, simply because “dog” and “cat” (resp. “the” and “a”, “room” and “bedroom”, etc...) have similar semantic and grammatical roles.

There are many approaches that have been proposed to address these two issues, and we will briefly explain in Section 1.2 the relations between the approach proposed here and some of these earlier approaches. We will first discuss what is the basic idea of the proposed approach. A more formal presentation will follow in Section 2, using an implementation of these ideas that relies on shared-parameter multi-layer neural networks. Another contribution of this paper concerns the challenge of training such very large neural networks (with millions of parameters) for very large data sets (with millions or tens of millions of examples). Finally, an important contribution of this paper is to show that training such large-scale model is expensive but feasible, scales to large contexts, and yields good comparative results (Section 4).

Many operations in this paper are in matrix notation, with lower case \(v\) denoting a column vector and \(v^T\) its transpose, \(A_j\) the j-th row of a matrix \(A\), and \(x \cdot y = x^T y\).

### 1.1 Fighting the Curse of Dimensionality with Distributed Representations

In a nutshell, the idea of the proposed approach can be summarized as follows:

1. Associate with each word in the vocabulary a distributed word feature vector (a real-valued vector in \(\mathbb{R}^m\)),
2. Express the joint probability function of word sequences in terms of the feature vectors of these words in the sequence, and
3. Learn simultaneously the word feature vectors and the parameters of that probability function.

The feature vector represents different aspects of the word: each word is associated with a point in a vector space. The number of features (e.g. \(m = 30, 60\) or 100 in the experiments) is much smaller than the size of the vocabulary (e.g. 17,000). The probability function is expressed as a product of conditional probabilities of the next word given the previous ones, (e.g. using a multi-layer neural network to predict the next word given the previous ones, in the experiments). This function has parameters that can be iteratively tuned in order to maximize the log-likelihood of the training data or a regularized criterion, e.g. by adding a weight decay penalty. The feature vectors associated with each word are learned, but they could be initialized using prior knowledge of semantic features.

Why does it work? In the previous example, if we knew that dog and cat played similar roles (semantically and syntactically), and similarly for (the, a), (bedroom, room), (is, was),

1. n-grams with n up to 5 (i.e. 4 words of context) have been reported, though, but due to data scarcity, most predictions are made with a much shorter context.
2. Like in ridge regression, the squared norm of the parameters is penalized.

(running, walking), we could naturally generalize (i.e. transfer probability mass) from

"The cat is walking in the bedroom"  
to  
"A dog was running in a room"  

and likewise to:  
"The cat is running in a room"  
"A dog is walking in a bedroom"  
"The dog was walking in the room"  
...

and many other combinations. In the proposed model, it will so generalize because “similar” words are expected to have a similar feature vector, and because the probability function is a smooth function of these feature values, a small change in the features will induce a small change in the probability. Therefore, the presence of only one of the above sentences in the training data will increase the probability, not only of that sentence, but also of its combinatorial number of “neighbors” in sentence space (as represented by sequences of feature vectors).

### 1.2 Relation to Previous Work

The idea of using neural networks to model high-dimensional discrete distributions has already been found useful to learn the joint probability of \(Z_1, \dots, Z_n\), a set of random variables where each is possibly of a different nature (Bengio and Bengio, 2000a,b). In that model, the joint probability is decomposed as a product of conditional probabilities

\[ P(Z_1 = z_1, \dots, Z_n = z_n) = \prod_i P(Z_i = z_i \mid g_i(Z_{i-1} = z_{i-1}, Z_{i-2} = z_{i-2}, \dots, Z_1 = z_1)), \]

where \(g(.)\) is a function represented by a neural network with a special left-to-right architecture, with the i-th output block \(g_i()\) computing parameters for expressing the conditional distribution of \(Z_i\) given the value of the previous Z’s, in some arbitrary order. Experiments on four UCI data sets show this approach to work comparatively very well (Bengio and Bengio, 2000a,b). Here we must deal with data of variable length, like sentences, so the above approach must be adapted. Another important difference is that here, all the \(Z_i\) (word at i-th position) refer to the same type of object (a word). The model proposed here therefore introduces a sharing of parameters across time – the same \(g_i\) is used across time – that is, and across input words at different positions. It is a successful large-scale application of the same idea, along with the (old) idea of learning a distributed representation for symbolic data, that was advocated in the early days of connectionism (Hinton, 1986, Elman, 1990). More recently, Hinton’s approach was improved and successfully demonstrated on learning several symbolic relations (Paccanaro and Hinton, 2000). The idea of using neural networks for language modeling is not new either (e.g. Miikkulainen and Dyer, 1991). In contrast, here we push this idea to a large scale, and concentrate on learning a statistical model of the distribution of word sequences, rather than learning the role of words in a sentence. The approach proposed here is also related to previous proposals of character-based text compression using neural networks to predict the probability of the next character (Schmidhuber, 1996). The idea of using a neural network for language modeling has also been independently proposed by Xu and Rudnicky (2000), although experiments are with networks without hidden units and a single input word, which limit the model to essentially capturing unigram and bigram statistics.

The idea of discovering some similarities between words to obtain generalization from training sequences to new sequences is not new. For example, it is exploited in approaches that are based on learning a clustering of the words (Brown et al., 1992, Pereira et al., 1993, Niesler et al., 1998, Baker and McCallum, 1998): each word is associated deterministically or probabilistically with a discrete class, and words in the same class are similar in some respect. In the model proposed here, instead of characterizing the similarity with a discrete random or deterministic variable (which corresponds to a soft or hard partition of the set of words), we use a continuous real-vector for each word, i.e. a learned distributed feature vector, to represent similarity between words. The experimental comparisons in this paper include results obtained with class-based n-grams (Brown et al., 1992, Ney and Kneser, 1993, Niesler et al., 1998).

The idea of using a vector-space representation for words has been well exploited in the area of information retrieval (for example see work by Schutze, 1993), where feature vectors for words are learned on the basis of their probability of co-occurring in the same documents (Latent Semantic Indexing, see Deerwester et al., 1990). An important difference is that here we look for a representation for words that is helpful in representing compactly the probability distribution of word sequences from natural language text. Experiments suggest that learning jointly the representation (word features) and the model is very useful. We tried (unsuccessfully) using as fixed word features for each word \(w\) the first principal components of the co-occurrence frequencies of \(w\) with the words occurring in text around the occurrence of \(w\). This is similar to what has been done with documents for information retrieval with LSI. The idea of using a continuous representation for words has however been exploited successfully by Bellegarda (1997) in the context of an n-gram based statistical language model, using LSI to dynamically identify the topic of discourse.

The idea of a vector-space representation for symbols in the context of neural networks has also previously been framed in terms of a parameter sharing layer, (e.g. Riis and Krogh, 1996) for secondary structure prediction, and for text-to-speech mapping (Jensen and Riis, 2000).

## 2. A Neural Model

The training set is a sequence \(w_1, \dots, w_T\) of words \(w_t \in V\), where the vocabulary \(V\) is a large but finite set. The objective is to learn a good model \(f(w_t, \dots, w_{t-n+1}) = P(w_t \mid w_{t-1}, \dots, w_1)\), in the sense that it gives high out-of-sample likelihood. Below, we report the geometric average of \(1/P(w_t \mid w_{t-1}, \dots, w_1)\), also known as perplexity, which is also the exponential of the average negative log-likelihood. The only constraint on the model is that for any choice of \(w_{t-1}, \dots, w_1\),

\[ \sum_{i=1}^{|V|} f(i, w_{t-1}, \dots, w_{t-n+1}) = 1, \]

with \(f > 0\). By the product of these conditional probabilities, one obtains a model of the joint probability of sequences of words.

We decompose the function \(f(w_t, \dots, w_{t-n+1}) = P(w_t \mid w_{t-1}, \dots, w_1)\) in two parts:

1. A mapping \(C\) from any element \(i\) of \(V\) to a real vector \(C(i) \in \mathbb{R}^m\). It represents the distributed feature vectors associated with each word in the vocabulary. In practice, \(C\) is represented by a \(|V| \times m\) matrix of free parameters.
2. The probability function over words, expressed with \(C\): a function \(g\) maps an input sequence of feature vectors for words in context, \((C(w_{t-n+1}), \dots, C(w_{t-1}))\), to a conditional probability distribution over words in \(V\) for the next word \(w_t\). The output of \(g\) is a vector whose i-th element estimates the probability \(P(w_t = i \mid w_{t-1}, \dots, w_1)\) as in Figure 1.

\[ f(i, w_{t-1}, \dots, w_{t-n+1}) = g(i, C(w_{t-1}), \dots, C(w_{t-n+1})) \]

The function \(f\) is a composition of these two mappings (\(C\) and \(g\)), with \(C\) being shared across all the words in the context. With each of these two parts are associated some parameters.

![Figure 1: Neural architecture: \(f(i, w_{t-1}, \dots, w_{t-n+1}) = g(i, C(w_{t-1}), \dots, C(w_{t-n+1}))\) where \(g\) is the neural network and \(C(i)\) is the i-th word feature vector.](path_to_figure)

The parameters of the mapping \(C\) are simply the feature vectors themselves, represented by a \(|V| \times m\) matrix \(C\) whose row \(i\) is the feature vector \(C(i)\) for word \(i\). The function \(g\) may be implemented by a feed-forward or recurrent neural network or another parameterized function, with parameters \(\omega\). The overall parameter set is \(\theta = (C, \omega)\).

Training is achieved by looking for \(\theta\) that maximizes the training corpus penalized log-likelihood:

\[ L = \frac{1}{T} \sum_t \log f(w_t, w_{t-1}, \dots, w_{t-n+1}; \theta) + R(\theta), \]

where \(R(\theta)\) is a regularization term. For example, in our experiments, \(R\) is a weight decay penalty applied only to the weights of the neural network and to the \(C\) matrix, not to the biases. (_3_. The biases are the additive parameters of the neural network, such as \(b\) and \(d\) in the equation below.)

In the above model, the number of free parameters only scales linearly with \(|V|\), the number of words in the vocabulary. It also only scales linearly with the order \(n\); the scaling factor could be reduced to sub-linear if more sharing structure were introduced, e.g., using a time-delay neural network or a recurrent neural network (or a combination of both).

In most experiments below, the neural network has one hidden layer beyond the word features mapping, and optionally, direct connections from the word features to the output. Therefore, there are really two hidden layers: the shared word features layer \(C\), which has no non-linearity (it would not add anything useful), and the ordinary hyperbolic tangent hidden layer. More precisely, the neural network computes the following function, with a softmax output layer, which guarantees positive probabilities summing to 1:

\[ P(w_t \mid w_{t-1}, \dots, w_{t-n+1}) = \frac{e^{y_{w_t}}}{\sum_i e^{y_i}}. \]

The \(y_i\) are the unnormalized log-probabilities for each output word \(i\), computed as follows, with parameters \(b\), \(W\), \(U\), \(d\) and \(H\):

\[ y = b + Wx + U \tanh(d + Hx) \]

where the hyperbolic tangent \(\tanh\) is applied element by element, \(W\) is optionally zero (no direct connections), and \(x\) is the word features layer activation vector, which is the concatenation of the input word features from the matrix \(C\):

\[ x = (C(w_{t-1}), C(w_{t-2}), \dots, C(w_{t-n+1})). \]

Let \(h\) be the number of hidden units, and \(m\) the number of features associated with each word. When no direct connections from word features to outputs are desired, the matrix \(W\) is set to 0. The free parameters of the model are the output biases \(b\) (with \(|V|\) elements), the hidden layer biases \(d\) (with \(h\) elements), the hidden-to-output weights \(U\) (a \(|V| \times h\) matrix), the word features to output weights \(W\) (a \(|V| \times (n-1)m\) matrix), the hidden layer weights \(H\) (a \(h \times (n-1)m\) matrix), and the word features \(C\) (a \(|V| \times m\) matrix):

\[ \theta = (b, d, W, U, H, C). \]

The number of free parameters is \(|V|(1 + nm + h) + h(1 + (n-1)m)\). The dominating factor is \(|V|(nm + h)\). Note that in theory, if there is a weight decay on the weights \(W\) and \(H\) but not on \(C\), then \(W\) and \(H\) could converge towards zero while \(C\) would blow up. In practice we did not observe such behavior when training with stochastic gradient ascent.

Stochastic gradient ascent on the neural network consists in performing the following iterative update after presenting the t-th word of the training corpus:

\[ \theta \leftarrow \theta + \epsilon \frac{\partial \log P(w_t \mid w_{t-1}, \dots, w_{t-n+1})}{\partial \theta} \]

where \(\epsilon\) is the “learning rate”. Note that a large fraction of the parameters needs not be updated or visited after each example: the word features \(C(j)\) of all words \(j\) that do not occur in the input window.

**Mixture of models:** In our experiments (see Section 4) we have found improved performance by combining the probability predictions of the neural network with those of an interpolated trigram model, either with a simple fixed weight of 0.5, a learned weight (maximum likelihood on the validation set) or a set of weights that are conditional on the frequency of the context (using the same procedure that combines trigram, bigram, and unigram in the interpolated trigram, which is a mixture).

## 3. Parallel Implementation

Although the number of parameters scales nicely, i.e., linearly with the size of the input window and linearly with the size of the vocabulary, the amount of computation required for obtaining the output probabilities is much greater than that required from n-gram models. The main reason is that with n-gram models, obtaining a particular \(P(w_t \mid w_{t-1}, \dots, w_{t-n+1})\) does not require the computation of the probabilities for all the words in the vocabulary, because of the easy normalization (performed when training the model) enjoyed by the linear combinations of relative frequencies. The main computational bottleneck with the neural implementation is the computation of the activations of the output layer.

Running the model (both training and testing) on a parallel computer is a way to reduce computation time. We have explored parallelization on two types of platforms: shared-memory processor machines and Linux clusters with a fast network.

### 3.1 Data-Parallel Processing

In the case of a shared-memory processor, parallelization is easily achieved, thanks to the very low communication overhead between processors through the shared memory. In that case, we have chosen a data-parallel implementation in which each processor works on a different subset of the data. Each processor computes the gradient for its examples and performs stochastic gradient updates on the parameters of the model, which are simply stored in a shared-memory area. Our first implementation was extremely slow and relied on synchronization commands to ensure that each processor would not write at the same time as another one in one of the parameter subsets. Most of the cycles of each processor were spent waiting for another processor to release a lock on the write access to the parameters.

Instead, we have chosen an asynchronous implementation where each processor can write at any time in the shared-memory area. Sometimes, part of an update on the parameter vector by one of the processors is lost, being overwritten by the update of another processor, and this introduces a bit of noise in the parameter updates. However, this noise seems to be very small and did not apparently slow down training.

Unfortunately, large shared-memory parallel computers are very expensive and their processor speed tends to lag behind mainstream CPUs that can be connected in clusters. We have thus been able to obtain much faster training on fast network clusters.

### 3.2 Parameter-Parallel Processing

If the parallel computer is a network of CPUs, we generally can’t afford to frequently exchange all the parameters among the processors, because that represents tens of megabytes (almost 100 megabytes in the case of our largest network), which would take too much time through a local network. Instead, we have chosen to parallelize across the parameters, in particular the parameters of the output units, because that is where the vast majority of the computation is taking place in our architecture. Each CPU is responsible for the computation of the unnormalized probability for a subset of the outputs and performs the updates for the corresponding output unit parameters (weights going into that unit). This strategy allowed us to perform a parallelized stochastic gradient ascent with a negligible communication overhead. The CPUs essentially need to communicate two pieces of information: (1) the normalization factor of the output softmax, and (2) the gradients on the hidden layer (denoted \(a\) below) and word feature layer (denoted \(x\)). All the CPUs duplicate the computations that precede the computation of the output units' activations, i.e., the selection of word features and the computation of the hidden layer activation \(a\), as well as the corresponding back-propagation and update steps. However, these computations are a negligible part of the total computation for our networks.

For example, consider the following architecture used in the experiments on the AP (Associated Press) news data: the vocabulary size is \(|V| = 17,964\), the number of hidden units is \(h = 60\), the order of the model is \(n = 6\), and the number of word features is \(m = 100\). The total number of numerical operations to process a single training example is approximately \(|V|(1 + nm + h) + h(1 + nm) + nm\) (where the terms correspond respectively to the computations of the output units, hidden units, and word features).
