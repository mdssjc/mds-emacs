# MDS - Emacs

O projeto tem como objetivo ser um modelo de configuração do _Emacs_ para o dia a dia do programador nas seguintes tecnologias: _Lisp_, _Haskell_, _C_, _Java_, _Web_ e Escrita (_Latex_, _R_, _Markdown_, Texto e etc...) - voltado para os amantes em personalização de ferramentas.

Versão do _Emacs_: _GNU Emacs 27.0.50_

# Instalação

Procedimento para a instalação do projeto.

## Instalação do Emacs

No _Linux_:

```
git clone https://github.com/emacs-mirror/emacs
./autogen.sh all
./configure --enable-link-time-optimization --with-modules CFLAGS='-O3'
make
sudo make install
```

No _Windows_:

```
browse to:   https://alpha.gnu.org/gnu/emacs/pretest/windows/emacs-27/
download:    <emacs-27.0.50-snapshot-2017-11-30-x86_64.zip | emacs-27.0.50-snapshot-2017-11-30-i686.zip>
extract to:  X:\emacs
shortcut to: X:\emacs\bin\runemacs.exe
```

## Clonagem do Projeto

```
mkdir ~/.emacs.d
cd ~/.emacs.d
git clone https://github.com/mdssjc/mds-emacs
```

## Dependências

Alguns pacotes dependem de _softwares_ externos:

* _Magit_: [Git](https://git-scm.com "Git")
* _RG_: [Ripgrep](https://github.com/BurntSushi/ripgrep "Ripgrep")
* _LanguageTool_: [LanguageTool](https://www.languagetool.org "LanguageTool")
* _Pandoc_: [Pandoc](http://pandoc.org "Pandoc")

# O que fazer?

Durante o desenvolvimento, o projeto servirá de estudo para o conhecimento da plataforma, entretanto recomendo a utilização do _Spacemacs_ como _starter kit_.

[Spacemacs](https://github.com/syl20bnr/spacemacs "Spacemacs")

## Etapas

Próximas evoluções:

* Acompanhar a evolução do _Java Meghanada_.
* Evoluir o _Org-mode_.
* Melhorar a legibilidade do código.
* Melhorar a documentação do _README_ e código.
* Incrementar as funcionalidades dos pacotes _Web_ e _Terminal_.
* Incluir suporte para _Guile_, _LFE_ e _R_.

# Projeto

Descrição da composição do projeto.

## Núcleo

* Quebra de Linha: acima, abaixo e entre a posição do cursor.
* Texto: apresentação de formas de cadeias de caracteres.

## Pacotes

Conjunto de pacotes para o ambiente:

* Estético: estilo ergonômico e sem distrações/ruídos - tema _dark_ e linha de _status_ com ícones.
* Atalhos: atalhos utilizados no ambiente.
* Sintaxe: conjuntos de funcionalidades para facilitar a codificação e escrita.
   * Autocompletar;
   * Correção;
   * Abreviação; e
   * Template.
* Semântico: analisa o texto e/ou código com frases e/ou expressões incorretas.
* Pragmático: organização do trabalho com _org-mode_.
* Estrutura: conjunto estrutural de melhorias/funcionalidades para o ambiente.
* Notícias: _feeds_ e _Twitter_.
* Linguagens de Programação: _Lisp_ (dialetos _ELisp_ - _Emacs Lisp_, _Racket_ e _Clojure_), _Haskell_, _C_ (com _Irony_) e _Java_ (_Meghanada_, _JDEE_ ou _Eclim_).
* Linguagens de Marcação: _Markdown_.
* Linguagens _Web_.

## Comandos

Listagem dos comandos fundamentais:

* `F1` - Ajuda do _Emacs_ (funções, variáveis, atalhos...)
* `F2` - Alterações de coluna
* `F3` - Inicia/Incrementa a gravação de macro
* `F4` - Finaliza/Executa a macro
* `F5` - _Toggle_
* `F6` - Sintático
* `F7` - Semântico
* `F8` - Pragmático
* `F9` - Configurações do modo maior
* `F10` - Comandos LaCarte
* `F11` - Ativa o _Fullscreen_
* `F12` - _IBuffer_
* `Ctrl + x` - comandos para o ambiente
* `Ctrl + c` - comandos para o modo maior
* `Alt  + x` - prompt de comandos
* `Win`    - atalhos especiais ao ambiente/modo maior
* `Alt Gr` - inserção de caracteres especiais
* `Menu`   - prompt de comandos
* `Ctrl + Alt + Enter` - super comando
