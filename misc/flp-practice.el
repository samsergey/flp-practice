(defface haskell-type-declaration-face
  '((t (:foreground "LightSeaGreen" :slant italic :weight bold :underline t))) "haskell-type-declaration-face")
(defface haskell-type-declaration-multline-face
  '((t (:inherit haskell-type-declaration-face :underline nil))) "haskell-type-declaration-multline-face")
(defface haskell-package-face '((t (:foreground "#599"))) "haskell-package-face")
(defface number-face '((t (:foreground "khaki"))) "haskell-package-face")
(defface haskell-binding-face '((t (:foreground "black" :weight bold))) "haskell-binding-face")
(defface haskell-guard-face '((t (:weight normal))) "haskell-guard-face")
(defface haskell-dot-face '((t (:weight extrabold))) "haskell-dot-face")
(defface haskell-local-definition-face '((t (:foreground "#a00" :weight normal :slant italic))) "haskell-local-definition-face")
(defface haskell-prelude-function-face '((t (:foreground "white"))) "haskell-prelude-function-face")
(defface haskell-prelude-type-face '((t (:foreground "DarkSeaGreen" :weight bold))) "haskell-prelude-type-face")
(defface haskell-prelude-class-face '((t (:foreground "aquamarine" :weight bold))) "haskell-prelude-class-face")
(defface haskell-prelude-constructor-face '((t (:foreground "DarkSeaGreen" :weight normal))) "haskell-prelude-constructor-face")


(setq kwd
      "\\<abs\\>\\|\\<acos\\>\\|\\<acosh\\>\\|\\<all\\>\\|\\<and\\>\\|\\<any\\>\\|\\<appendFile\\>\\|\\<asin\\>\\|\\<asinh\\>\\|\\<asTypeOf\\>\\|\\<atan\\>\\|\\<atanh\\>\\|\\<break\\>\\|\\<ceiling\\>\\|\\<compare\\>\\|\\<concat\\>\\|\\<concatMap\\>\\|\\<const\\>\\|\\<cos\\>\\|\\<cosh\\>\\|\\<curry\\>\\|\\<cycle\\>\\|\\<decodeFloat\\>\\|\\<div\\>\\|\\<divMod\\>\\|\\<drop\\>\\|\\<dropWhile\\>\\|\\<either\\>\\|\\<elem\\>\\|\\<encodeFloat\\>\\|\\<enumFrom\\>\\|\\<enumFromThen\\>\\|\\<enumFromThenTo\\>\\|\\<enumFromTo\\>\\|\\<error\\>\\|\\<errorWithoutStackTrace\\>\\|\\<even\\>\\|\\<exp\\>\\|\\<exponent\\>\\|\\<fail\\>\\|\\<filter\\>\\|\\<flip\\>\\|\\<floatDigits\\>\\|\\<floatRadix\\>\\|\\<floatRange\\>\\|\\<floor\\>\\|\\<fmap\\>\\|\\<fold\\>\\|\\<foldl\\>\\|\\<foldMap\\>\\|\\<foldr\\>\\|\\<fromEnum\\>\\|\\<fromInteger\\>\\|\\<fromIntegral\\>\\|\\<fromRational\\>\\|\\<fst\\>\\|\\<gcd\\>\\|\\<getChar\\>\\|\\<getContents\\>\\|\\<getLine\\>\\|\\<head\\>\\|\\<id\\>\\|\\<init\\>\\|\\<interact\\>\\|\\<ioError\\>\\|\\<isDenormalized\\>\\|\\<isIEEE\\>\\|\\<isInfinite\\>\\|\\<isNaN\\>\\|\\<isNegativeZero\\>\\|\\<iterate\\>\\|\\<last\\>\\|\\<lcm\\>\\|\\<length\\>\\|\\<lex\\>\\|\\<lines\\>\\|\\<log\\>\\|\\<logBase\\>\\|\\<lookup\\>\\|\\<map\\>\\|\\<mapM\\>\\|\\<mappend\\>\\|\\<max\\>\\|\\<maxBound\\>\\|\\<maximum\\>\\|\\<maybe\\>\\|\\<mconcat\\>\\|\\<mempty\\>\\|\\<min\\>\\|\\<minBound\\>\\|\\<minimum\\>\\|\\<mod\\>\\|\\<negate\\>\\|\\<not\\>\\|\\<notElem\\>\\|\\<null\\>\\|\\<odd\\>\\|\\<or\\>\\|\\<otherwise\\>\\|\\<pi\\>\\|\\<pred\\>\\|\\<print\\>\\|\\<product\\>\\|\\<properFraction\\>\\|\\<pure\\>\\|\\<putChar\\>\\|\\<putStr\\>\\|\\<putStrLn\\>\\|\\<quot\\>\\|\\<quotRem\\>\\|\\<read\\>\\|\\<readFile\\>\\|\\<readIO\\>\\|\\<readList\\>\\|\\<readListPrec\\>\\|\\<readLn\\>\\|\\<readParen\\>\\|\\<readPrec\\>\\|\\<reads\\>\\|\\<readsPrec\\>\\|\\<realToFrac\\>\\|\\<recip\\>\\|\\<rem\\>\\|\\<repeat\\>\\|\\<replicate\\>\\|\\<return\\>\\|\\<reverse\\>\\|\\<round\\>\\|\\<scaleFloat\\>\\|\\<scanl\\>\\|\\<scanr\\>\\|\\<seq\\>\\|\\<sequence\\>\\|\\<sequenceA\\>\\|\\<show\\>\\|\\<showChar\\>\\|\\<showList\\>\\|\\<showParen\\>\\|\\<shows\\>\\|\\<showsPrec\\>\\|\\<showString\\>\\|\\<significand\\>\\|\\<signum\\>\\|\\<sin\\>\\|\\<sinh\\>\\|\\<snd\\>\\|\\<span\\>\\|\\<splitAt\\>\\|\\<sqrt\\>\\|\\<subtract\\>\\|\\<succ\\>\\|\\<sum\\>\\|\\<tail\\>\\|\\<take\\>\\|\\<takeWhile\\>\\|\\<tan\\>\\|\\<tanh\\>\\|\\<toEnum\\>\\|\\<toInteger\\>\\|\\<toList\\>\\|\\<toRational\\>\\|\\<traverse\\>\\|\\<truncate\\>\\|\\<uncurry\\>\\|\\<undefined\\>\\|\\<unlines\\>\\|\\<until\\>\\|\\<unwords\\>\\|\\<unzip\\>\\|\\<userError\\>\\|\\<words\\>\\|\\<writeFile\\>\\|\\<zip\\>\\|\\<zipWith\\>\\|\\<empty\\>")

(setq constrs
      "\\<True\\>\\|\\<False\\>\\|\\<Nothing\\>\\|\\<Just\\>\\|\\<GT\\>\\|\\<LT\\>\\|\\<EQ\\>\\|\\<Left\\>\\|\\<Right\\>")

(setq types
      "\\<Bool\\>\\|\\<Char\\>\\|\\<Data\\>\\|\\<Double\\>\\|\\<Either\\>\\|\\<Exception\\>\\|\\<Float\\>\\|\\<Int\\>\\|\\<Integer\\>\\|\\<Jn\\>\\|\\<Jp\\>\\|\\<Maybe\\>\\|\\<Ordering\\>\\|\\<Ratio\\>\\|\\<Read\\>\\|\\<Stack\\>\\|\\<State\\>\\|\\<String\\>\\|\\<Text\\>\\|\\<Type\\>\\|\\<Types\\>\\|\\<Word\\>")

(setq classes
      "\\<Applicative\\>\\|\\<Bounded\\>\\|\\<Enum\\>\\|\\<Eq\\>\\|\\<Floating\\>\\|\\<Foldable\\>\\|\\<Fractional\\>\\|\\<Functor\\>\\|\\<Integral\\>\\|\\<Monad\\>\\|\\<Monoid\\>\\|\\<Num\\>\\|\\<Ord\\>\\|\\<Rational\\>\\|\\<Read\\>\\|\\<Show\\>\\|\\<Traversable\\>")


(add-hook 'haskell-mode-hook
	  '(lambda ()
	     (font-lock-add-keywords 
	      nil 
	      `(
;		(" \\(<-\\|=\\|->\\) " . 'haskell-binding-face)
		("[{,] *\\(\\w+\\) ::" 1 'haskell-definition-face t)
	  
;		("\\\\.+?\\(->\\)" 1 'haskell-binding-face t)
;		("\\\\" 0 'haskell-binding-face t)
;		("\\<_" 0 'default t)

;		("\\(where\\|let\\) +\\(\\w+\\)[^=`]*?=[^=]" 2 'haskell-local-definition-face t)
;		("\\(where\\|let\\) +\\(([^=
;]+\\)=[^=]" 2 'haskell-local-definition-face t)
;		("^ +\\(\\w+\\)[^=`
;]+=[^=]" 1 'haskell-local-definition-face t)
;		("^ +\\(([^=
;]+\\)=[^=]" 1 'haskell-local-definition-face t)
;		("^ +[^`|=]+\\(`\\w+`\\)[^=]+ *=[^=]" 1 'haskell-local-definition-face t)
;		("^ +\\(\\w+\\|[(,)':]\\)+ +<-" 1 'haskell-local-definition-face t)
;		("[(,)]\\|\\[\\|\\]" 0 'default t)
;		("[^:]\\(:\\)[^:]" 1 'haskell-constructor-face t)

;		("\\<where\\|let\\>" 0 'haskell-keyword-face t)
;		("\\<[[:digit:]]+\\([.][[:digit:]]+\\)?\\([eE]-?[[:digit:]]\\{1,3\\}\\)?" 0 'number-face t)
		(" [.] " 0 'haskell-dot-face t)
		("\\([[:upper:]][[:alnum:]]+\\([.][[:upper:]][[:alnum:]]*\\)*\\)[.][[:alnum:]]" 1 'haskell-package-face t)
		("^import +\\([[:upper:]][[:alnum:]]+\\([.][[:upper:]][[:alnum:]]*\\)*\\)" 1 'haskell-package-face t)
		("^import .*? as \\(\\w+\\)" 1 'haskell-package-face t)
;		(" | +\\(.*?\\) = " 1 'haskell-guard-face t)
;		(" | +\\(.*?\\) -> " 1 'haskell-guard-face t)
		(,kwd . 'haskell-prelude-function-face)
		(,types . 'haskell-prelude-type-face)
		(,classes . 'haskell-prelude-class-face)
		(,constrs . 'haskell-prelude-constructor-face)
		("^\\(\\(\\w\\|[_']\\)+\\|([<>!@#$%&*+=|/?.'-]+)\\)\\(, *\\(\\w+\\|([<>!@#$%&*+=|/?.'-]+)\\)\\)* :: [^
]+$" 0 'haskell-type-declaration-face t)
		("^ +\\(\\(=>\\|->\\) [^
]+\\)$" 1 'haskell-type-declaration-multline-face t)
;		("-- .*?$" 0 'font-lock-comment-face t)
		))))
