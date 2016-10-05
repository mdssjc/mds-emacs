# MDS - Emacs

O projeto tem como objetivo ser um modelo de configuração do *Emacs* para o dia a dia do programador nas seguintes tecnologias: *Lisp*, *Java*, *C*, *Web* e Escrita (*Latex*, *Markdown*, Texto e etc...) - voltado para os amantes em personalização de ferramentas.

Versão do *Emacs*: 25.1.50

# Instalação

Procedimento para a instalação do projeto.

## Instalação do Emacs

```
git clone https://github.com/emacs-mirror/emacs
./autogen.sh all
./configure
make
sudo make install
```

## Clonagem do Projeto

```
mkdir ~/.emacs.d
cd ~/.emacs.d
git clone https://github.com/mdssjc/mds-emacs
```

# O que fazer?

Durante o desenvolvimento, o projeto servirá de estudo para o conhecimento da plataforma, entretanto recomendo a utilização do *Spacemacs* como *starter kit*.

[Spacemacs](https://github.com/syl20bnr/spacemacs)

## Etapas

Próximas evoluções:

* Acompanhar a evolução do *Meghanada*.
* Evoluir o *Org-mode*.

# Projeto

Descrição da composição do projeto.

## Núcleo

* Cursores: mover/duplicar e espaçar linha(s).
* Texto: apresentação de formas de cadeias de caracteres.

## Pacotes

Conjunto de pacotes para o ambiente:

* Estético: estilo ergonômico e sem distrações/ruídos - tema *dark*, linha de *status* com ícones.
* Sintaxe: conjuntos de funcionalidades para facilitar a codificação e escrita.
   * Autocompletar: sugestão de texto, conforme modo maior.
   * Correção: sugestão para palavras selecionadas, conforme dicionário (português por padrão).
   * Abreviação: abreviaturas de texto para expansão.
   * Template: blocos de construção de texto (linguagens de programação).
* Semântica: frase e/ou expressão incorreta.
   * Analisa o texto e/ou código.
* Pragmático: *org-mode*...
   * Organização do trabalho (em desenvolvimento).
* Estrutura: conjunto de facilidades ao editor.
   * Salva a última posição da seção.
   * Listagem dos documentos recentes.
   * Visualização da árvore de modificações no documento.
   * Sugestão e rótulo de atalhos.
   * Seleção de partes do *buffer*.
   * Visualizador de arquivos.
   * Cliente *Git*.
   * Pacotes *Abo-abo*: *Avy*, *Ace-Window*, *Ivy*, *Swiper*, *Counsel* e *Hydra*.
   * Ferramenta *Ripgrep*.
* Linguagens de Programação:
   * *Lisp*: dialetos *ELisp* (*Emacs Lisp*) e *Racket*.
   * *Java*: *Meghanada* (em teste).
   * *C*: (sem configurações).
* Linguagens de Marcação:
   * *Markdown*: arquivos .md.
   * *Latex*: (sem configurações).
   * *XML*: (sem configurações).
* Linguagens *Web*: em desenvolvimento.

## Comandos

Listagem dos comandos fundamentais:

* `F1` - Ajuda do *Emacs* (funções, variáveis, atalhos...)
* `F2` - Alterações de coluna
* `F3` - Inicia/Incrementa a gravação de macro
* `F4` - Finaliza/Executa a macro
* `Ctrl + x` - configurações e utilitários do ambiente
* `Ctrl + c` - comandos para o modo maior atual
* `Alt  + x` - prompt de comandos
* `Win` - comando especial para o modo maior (em revisão)
* `F5` - *Toggle* Global
* `F6` - *Toggle* do Modo Maior
* `F7` - Aplicações Interna
* `F8` - Aplicações Externa
* `F9` - Planejamento (???)
