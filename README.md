# MDS - Emacs

O projeto tem como objetivo ser um modelo de configuração do *Emacs* para o dia a dia do programador nas seguintes tecnologias: *Lisp*, *Haskell*, *C*, *Java*, *Web* e Escrita (*Latex*, *R*, *Markdown*, Texto e etc...) - voltado para os amantes em personalização de ferramentas.

Versão do *Emacs*: 26.0.50

# Instalação

Procedimento para a instalação do projeto.

## Instalação do Emacs

Versão principal:

```
git clone https://github.com/emacs-mirror/emacs
./autogen.sh all
./configure --enable-link-time-optimization --with-modules CFLAGS='-O3'
make
sudo make install
```

## Clonagem do Projeto

```
mkdir ~/.emacs.d
cd ~/.emacs.d
git clone https://github.com/mdssjc/mds-emacs
```

## Dependências

Alguns pacotes dependem de *softwares* externos:

* *Eclim*: *Eclipse*
* *LangTool*: *LangTool*
* *Magit*: *Git*

# O que fazer?

Durante o desenvolvimento, o projeto servirá de estudo para o conhecimento da plataforma, entretanto recomendo a utilização do *Spacemacs* como *starter kit*.

[Spacemacs](https://github.com/syl20bnr/spacemacs)

## Etapas

Próximas evoluções:

* Acompanhar a evolução do *Java Meghanada*.
* Evoluir o *Org-mode*.
* Melhorar a legibilidade do código.
* Melhorar a documentação do *README* e código.
* Incrementar as funcionalidades dos pacotes *Web* e *Terminal*.
* Incluir suporte para *Guile*, *LFE* e *R*.

# Projeto

Descrição da composição do projeto.

## Núcleo

* Quebra de Linha: acima, abaixo e entre a posição do cursor.
* Texto: apresentação de formas de cadeias de caracteres.

## Pacotes

Conjunto de pacotes para o ambiente:

* Estético: estilo ergonômico e sem distrações/ruídos - tema *dark* e linha de *status* com ícones.
* Atalhos: atalhos utilizados no ambiente.
* Sintaxe: conjuntos de funcionalidades para facilitar a codificação e escrita.
   * Autocompletar;
   * Correção;
   * Abreviação; e
   * Template.
* Semântico: analisa o texto e/ou código com frases e/ou expressões incorretas.
* Pragmático: organização do trabalho com *org-mode*.
* Estrutura: conjunto estrutural de melhorias/funcionalidades para o ambiente.
* Notícias: *feeds* e *Twitter*.
* Linguagens de Programação: *Lisp* (dialetos *ELisp* - *Emacs Lisp*, *Racket* e *Clojure*), *Haskell*, *C* (com *Irony*) e *Java* (*Meghanada*, *JDEE* ou *Eclim*).
* Linguagens de Marcação: *Markdown*.
* Linguagens *Web*.

## Comandos

Listagem dos comandos fundamentais:

* `F1` - Ajuda do *Emacs* (funções, variáveis, atalhos...)
* `F2` - Alterações de coluna
* `F3` - Inicia/Incrementa a gravação de macro
* `F4` - Finaliza/Executa a macro
* `F5` - *Toggle*
* `F6` - Sintático
* `F7` - Semântico
* `F8` - Pragmático
* `F9` - Configurações do modo maior
* `F10` - Comandos LaCarte
* `F11` - Ativa o *Fullscreen*
* `F12` - *IBuffer*
* `Ctrl + x` - comandos para o ambiente
* `Ctrl + c` - comandos para o modo maior
* `Alt  + x` - prompt de comandos
* `\` e `/`  - disparador de *key chords*
* `Win`    - atalhos especiais ao ambiente/modo maior
* `Alt Gr` - inserção de caracteres especiais
* `Menu`   - prompt de comandos
* `Ctrl + Alt + Enter` - super comando
