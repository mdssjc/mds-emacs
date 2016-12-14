# MDS - Emacs

O projeto tem como objetivo ser um modelo de configuração do *Emacs* para o dia a dia do programador nas seguintes tecnologias: *Lisp*, *Haskell*, *C*, *Java*, *Web* e Escrita (*Latex*, *Markdown*, Texto e etc...) - voltado para os amantes em personalização de ferramentas.

Versão do *Emacs*: 26.0.50

# Instalação

Procedimento para a instalação do projeto.

## Instalação do Emacs

```
git clone https://github.com/emacs-mirror/emacs
./autogen.sh all
./configure --with-modules
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

* Acompanhar a evolução do *Java Meghanada*.
* Evoluir o *Org-mode*.
* Melhorar a legibilidade do código.
* Melhorar a documentação do *README* e código.
* Incrementar as funcionalidades dos pacotes *Web* e *Terminal*.

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
* Linguagens de Programação: *Lisp* (dialetos *ELisp* - *Emacs Lisp*, *Racket* e *Clojure*), *Haskell*, *C* (com *Irony*) e *Java* (com *Meghanada* e *JDEE*).
* Linguagens de Marcação: *Markdown*.
* Linguagens *Web*: em desenvolvimento.

## Comandos

Listagem dos comandos fundamentais:

* `F1` - Ajuda do *Emacs* (funções, variáveis, atalhos...)
* `F2` - Alterações de coluna
* `F3` - Inicia/Incrementa a gravação de macro
* `F4` - Finaliza/Executa a macro
* `F5` - *Toggle* Global
* `F6` - *Toggle* do Modo Maior
* `F7` - Aplicações Interna
* `F8` - Aplicações Externa
* `F9` - Planejamento
* `F10` - Menu do Modo Maior
* `F11` - Ativa o *Fullscreen*
* `F12` - <não definido>
* `Ctrl + x` - comandos para o ambiente
* `Ctrl + c` - comandos para o modo maior
* `Alt  + x` - prompt de comandos
* `Win` - comandos e/ou atalhos especiais ao ambiente/modo maior
* `Ctrl + Alt + Enter` - super comando
