# Build NanoGPT

## Video

### Chapters

- [00:00:00](https://www.youtube.com/watch?v=l8pRSuU81PU&t=0s) intro: Let’s reproduce GPT-2 (124M) 
- [00:03:39](https://www.youtube.com/watch?v=l8pRSuU81PU&t=219s) exploring the GPT-2 (124M) OpenAI checkpoint 
- [00:13:47](https://www.youtube.com/watch?v=l8pRSuU81PU&t=827s) SECTION 1: implementing the GPT-2 nn.Module 
- [00:28:08](https://www.youtube.com/watch?v=l8pRSuU81PU&t=1688s) loading the huggingface/GPT-2 parameters 
- [00:31:00](https://www.youtube.com/watch?v=l8pRSuU81PU&t=1860s) implementing the forward pass to get logits 
- [00:33:31](https://www.youtube.com/watch?v=l8pRSuU81PU&t=2011s) sampling init, prefix tokens, tokenization 
- [00:37:02](https://www.youtube.com/watch?v=l8pRSuU81PU&t=2222s) sampling loop 
- [00:41:47](https://www.youtube.com/watch?v=l8pRSuU81PU&t=2507s) sample, auto-detect the device 
- [00:45:50](https://www.youtube.com/watch?v=l8pRSuU81PU&t=2750s) let’s train: data batches (B,T) → logits (B,T,C) 
- [00:52:53](https://www.youtube.com/watch?v=l8pRSuU81PU&t=3173s) cross entropy loss 
- [00:56:42](https://www.youtube.com/watch?v=l8pRSuU81PU&t=3402s) optimization loop: overfit a single batch 
- [01:02:00](https://www.youtube.com/watch?v=l8pRSuU81PU&t=3720s) data loader lite 
- [01:06:14](https://www.youtube.com/watch?v=l8pRSuU81PU&t=3974s) parameter sharing wte and lm_head 
- [01:13:47](https://www.youtube.com/watch?v=l8pRSuU81PU&t=4427s) model initialization: std 0.02, residual init 
- [01:22:18](https://www.youtube.com/watch?v=l8pRSuU81PU&t=4938s) SECTION 2: Let’s make it fast. GPUs, mixed precision, 1000ms 
- [01:28:14](https://www.youtube.com/watch?v=l8pRSuU81PU&t=5294s) Tensor Cores, timing the code, TF32 precision, 333ms 
- [01:39:38](https://www.youtube.com/watch?v=l8pRSuU81PU&t=5978s) float16, gradient scalers, bfloat16, 300ms 
- [01:48:15](https://www.youtube.com/watch?v=l8pRSuU81PU&t=6495s) torch.compile, Python overhead, kernel fusion, 130ms 
- [02:00:18](https://www.youtube.com/watch?v=l8pRSuU81PU&t=7218s) flash attention, 96ms 
- [02:06:54](https://www.youtube.com/watch?v=l8pRSuU81PU&t=7614s) nice/ugly numbers. vocab size 50257 → 50304, 93ms 
- [02:14:55](https://www.youtube.com/watch?v=l8pRSuU81PU&t=8095s) SECTION 3: hyperpamaters, AdamW, gradient clipping 
- [02:21:06](https://www.youtube.com/watch?v=l8pRSuU81PU&t=8466s) learning rate scheduler: warmup + cosine decay 
- [02:26:21](https://www.youtube.com/watch?v=l8pRSuU81PU&t=8781s) batch size schedule, weight decay, FusedAdamW, 90ms 
- [02:34:09](https://www.youtube.com/watch?v=l8pRSuU81PU&t=9249s) gradient accumulation 
- [02:46:52](https://www.youtube.com/watch?v=l8pRSuU81PU&t=10012s) distributed data parallel (DDP) 
- [03:10:21](https://www.youtube.com/watch?v=l8pRSuU81PU&t=11421s) datasets used in GPT-2, GPT-3, FineWeb (EDU) 
- [03:23:10](https://www.youtube.com/watch?v=l8pRSuU81PU&t=12190s) validation data split, validation loss, sampling revive 
- [03:28:23](https://www.youtube.com/watch?v=l8pRSuU81PU&t=12503s) evaluation: HellaSwag, starting the run 
- [03:43:05](https://www.youtube.com/watch?v=l8pRSuU81PU&t=13385s) SECTION 4: results in the morning! GPT-2, GPT-3 repro 
- [03:56:21](https://www.youtube.com/watch?v=l8pRSuU81PU&t=14181s) shoutout to llm.c, equivalent but faster code in raw C/CUDA 
- [03:59:39](https://www.youtube.com/watch?v=l8pRSuU81PU&t=14379s) summary, phew, build-nanogpt github repo 

## Environment(Windows 11)

- cuda toolkit 11.8
- conda. Installer link: https://mirrors.tuna.tsinghua.edu.cn/anaconda/archive/Anaconda3-2024.06-1-Windows-x86_64.exe

生成`~\.condarc`文件

```shell
conda config --set show_channel_urls yes
```

编辑`~\.condarc`：

```yaml
channels:
  - defaults
show_channel_urls: true
default_channels:
  - https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/main
  - https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/r
  - https://mirrors.tuna.tsinghua.edu.cn/anaconda/pkgs/msys2
custom_channels:
  conda-forge: https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud
  msys2: https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud
  bioconda: https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud
  menpo: https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud
  pytorch: https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud
  pytorch-lts: https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud
  simpleitk: https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud
  deepmodeling: https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud/
```

### Create environment and install pytorch

```shell
conda create -n deeplearning pytorch torchvision torchaudio pytorch-cuda=11.8 -c pytorch -c nvidia
```

```shell
conda activate deeplearning
```

### install other package

```shell
conda install jupyterlab
conda install transformers matplotlib
pip install tiktoken
```

##### optional

```shell
conda clean -i
```



## Reference

- https://cloud.tencent.com/developer/article/2405758
- https://mirror.tuna.tsinghua.edu.cn/help/anaconda/
- https://pytorch.org/