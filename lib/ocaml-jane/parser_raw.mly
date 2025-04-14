%start implementation
%start interface
%start parse_any_longident
%start parse_constr_longident
%start parse_core_type
%start parse_expression
%start parse_mod_ext_longident
%start parse_mod_longident
%start parse_module_expr
%start parse_module_type
%start parse_mty_longident
%start parse_pattern
%start parse_val_longident
%start toplevel_phrase
%start use_file
%token AMPERAMPER
%token AMPERSAND
%token AND
%token <string> ANDOP
%token AS
%token ASSERT
%token AT
%token ATAT
%token BACKQUOTE
%token BANG
%token BAR
%token BARBAR
%token BARRBRACKET
%token BEGIN
%token <char> CHAR
%token CLASS
%token COLON
%token COLONCOLON
%token COLONEQUAL
%token COLONGREATER
%token COLONRBRACKET
%token COMMA
%token <string * Location.t> COMMENT
%token CONSTRAINT
%token DO
%token <Docstrings.docstring> DOCSTRING
%token DONE
%token DOT
%token DOTDOT
%token DOTHASH
%token <string> DOTOP
%token DOWNTO
%token ELSE
%token END
%token EOF
%token EOL
%token EQUAL
%token EXCEPTION
%token EXCLAVE
%token EXTERNAL
%token FALSE
%token <string * char option> FLOAT
%token FOR
%token FUN
%token FUNCTION
%token FUNCTOR
%token GLOBAL
%token GREATER
%token GREATERRBRACE
%token GREATERRBRACKET
%token HASH
%token HASHLBRACE
%token HASHLPAREN
%token <string> HASHOP
%token <string * char option> HASH_FLOAT
%token <string * char option> HASH_INT
%token HASH_SUFFIX
%token IF
%token IN
%token INCLUDE
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token INHERIT
%token INITIALIZER
%token <string * char option> INT
%token KIND_ABBREV
%token KIND_OF
%token <string> LABEL
%token LAZY
%token LBRACE
%token LBRACELESS
%token LBRACKET
%token LBRACKETAT
%token LBRACKETATAT
%token LBRACKETATATAT
%token LBRACKETBAR
%token LBRACKETCOLON
%token LBRACKETGREATER
%token LBRACKETLESS
%token LBRACKETPERCENT
%token LBRACKETPERCENTPERCENT
%token LESS
%token LESSMINUS
%token LET
%token <string> LETOP
%token <string> LIDENT
%token LOCAL
%token LPAREN
%token MATCH
%token METHOD
%token MINUS
%token MINUSDOT
%token MINUSGREATER
%token MOD
%token MODULE
%token MUTABLE
%token NEW
%token NONREC
%token OBJECT
%token OF
%token ONCE
%token OPEN
%token <string> OPTLABEL
%token OR
%token OVERWRITE
%token PERCENT
%token PLUS
%token PLUSDOT
%token PLUSEQ
%token <string> PREFIXOP
%token PRIVATE
%token QUESTION
%token QUOTE
%token <string * Location.t * string * Location.t * string option> QUOTED_STRING_EXPR
%token <string * Location.t * string * Location.t * string option> QUOTED_STRING_ITEM
%token RBRACE
%token RBRACKET
%token REC
%token RPAREN
%token SEMI
%token SEMISEMI
%token SIG
%token STACK
%token STAR
%token <string * Location.t * string option> STRING
%token STRUCT
%token THEN
%token TILDE
%token TO
%token TRUE
%token TRY
%token TYPE
%token <string> UIDENT
%token UNDERSCORE
%token UNIQUE
%token VAL
%token VIRTUAL
%token WHEN
%token WHILE
%token WITH
%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI
%nonassoc FOR LET
%nonassoc below_WITH
%nonassoc FUNCTION WITH
%nonassoc AND
%nonassoc THEN
%nonassoc ELSE
%nonassoc LESSMINUS
%right COLONEQUAL
%nonassoc AS
%left BAR
%nonassoc below_COMMA
%left COMMA
%nonassoc below_FUNCTOR
%nonassoc FUNCTOR
%right MINUSGREATER
%right BARBAR OR
%nonassoc below_AMPERSAND
%right AMPERAMPER AMPERSAND
%nonassoc below_EQUAL
%left EQUAL GREATER INFIXOP0 LESS
%right AT ATAT INFIXOP1
%nonassoc below_LBRACKETAT
%nonassoc LBRACKETAT
%right COLONCOLON
%left INFIXOP2 MINUS MINUSDOT PLUS PLUSDOT PLUSEQ
%left INFIXOP3 MOD PERCENT STAR
%right INFIXOP4
%nonassoc prec_unboxed_product_kind
%nonassoc prec_unary_minus prec_unary_plus
%nonassoc prec_constant_constructor
%nonassoc prec_constr_appl
%nonassoc below_HASH
%nonassoc HASH HASH_SUFFIX
%left HASHOP
%nonassoc below_DOT
%nonassoc DOT DOTHASH DOTOP
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT HASHLBRACE HASHLPAREN HASH_FLOAT HASH_INT INT LBRACE LBRACELESS LBRACKET LBRACKETBAR LBRACKETCOLON LBRACKETPERCENT LIDENT LPAREN NEW OBJECT PREFIXOP QUOTED_STRING_EXPR STRING TRUE UIDENT
%type <unit> implementation
%type <unit> interface
%type <unit> parse_any_longident
%type <unit> parse_constr_longident
%type <unit> parse_core_type
%type <unit> parse_expression
%type <unit> parse_mod_ext_longident
%type <unit> parse_mod_longident
%type <unit> parse_module_expr
%type <unit> parse_module_type
%type <unit> parse_mty_longident
%type <unit> parse_pattern
%type <unit> parse_val_longident
%type <unit> toplevel_phrase
%type <unit> use_file
%%

option_BAR_:
  
    {} [@name none_BAR]
| BAR
    {} [@name some_BAR]

option_SEMI_:
  
    {} [@name none_SEMI]
| SEMI
    {} [@name some_SEMI]

option_constraint__:
  
    {} [@name none_constraint_]
| COLON core_type
    {} [@name some_constraint_]
| COLON tuple_type at_mode_expr
    {} [@name some_constraint_]
| COLON core_type COLONGREATER core_type
    {} [@name some_constraint_]
| COLON core_type COLONGREATER tuple_type at_mode_expr
    {} [@name some_constraint_]
| COLONGREATER core_type
    {} [@name some_constraint_]
| COLONGREATER tuple_type at_mode_expr
    {} [@name some_constraint_]
| at_mode_expr
    {} [@name some_constraint_]

option_jkind_constraint_:
  
    {} [@name none_jkind_constraint]
| jkind_constraint
    {} [@name some_jkind_constraint]

option_preceded_AS_mkrhs_LIDENT___:
  
    {} [@name none_preceded_AS_mkrhs_LIDENT__]
| AS LIDENT
    {} [@name some_preceded_AS_mkrhs_LIDENT__]

option_preceded_COLON_core_type__:
  
    {} [@name none_preceded_COLON_core_type_]
| COLON core_type
    {} [@name some_preceded_COLON_core_type_]

option_preceded_EQUAL_expr__:
  
    {} [@name none_preceded_EQUAL_expr_]
| EQUAL fun_expr
    {} [@name some_preceded_EQUAL_expr_]
| EQUAL FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {} [@name some_preceded_EQUAL_expr_]
| EQUAL STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {} [@name some_preceded_EQUAL_expr_]

option_preceded_EQUAL_module_type__:
  
    {} [@name none_preceded_EQUAL_module_type_]
| EQUAL module_type
    {} [@name some_preceded_EQUAL_module_type_]

option_preceded_EQUAL_pattern__:
  
    {} [@name none_preceded_EQUAL_pattern_]
| EQUAL pattern
    {} [@name some_preceded_EQUAL_pattern_]

option_preceded_EQUAL_seq_expr__:
  
    {} [@name none_preceded_EQUAL_seq_expr_]
| EQUAL seq_expr
    {} [@name some_preceded_EQUAL_seq_expr_]

option_type_constraint_:
  
    {} [@name none_type_constraint]
| type_constraint
    {} [@name some_type_constraint]

list_and_class_declaration_:
  
    {} [@name nil_and_class_declaration]
| AND list_attribute_ virtual_flag formal_class_parameters LIDENT class_fun_binding list_post_item_attribute_ list_and_class_declaration_
    {} [@name cons_and_class_declaration]

list_and_class_description_:
  
    {} [@name nil_and_class_description]
| AND list_attribute_ virtual_flag formal_class_parameters LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_
    {} [@name cons_and_class_description]

list_and_class_type_declaration_:
  
    {} [@name nil_and_class_type_declaration]
| AND list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
    {} [@name cons_and_class_type_declaration]

list_and_module_binding_:
  
    {} [@name nil_and_module_binding]
| AND list_attribute_ module_name_modal_at_mode_expr_ module_binding_body list_post_item_attribute_ list_and_module_binding_
    {} [@name cons_and_module_binding]

list_and_module_declaration_:
  
    {} [@name nil_and_module_declaration]
| AND list_attribute_ module_name COLON module_type optional_atat_modalities_expr list_post_item_attribute_ list_and_module_declaration_
    {} [@name cons_and_module_declaration]

list_attribute_:
  
    {} [@name nil_attribute]
| attribute list_attribute_
    {} [@name cons_attribute]

list_generic_and_type_declaration_type_kind__:
  
    {} [@name nil_generic_and_type_declaration_type_kind_]
| AND list_attribute_ type_parameters LIDENT option_jkind_constraint_ type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_kind__
    {} [@name cons_generic_and_type_declaration_type_kind_]

list_generic_and_type_declaration_type_subst_kind__:
  
    {} [@name nil_generic_and_type_declaration_type_subst_kind_]
| AND list_attribute_ type_parameters LIDENT option_jkind_constraint_ COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_ list_generic_and_type_declaration_type_subst_kind__
    {} [@name cons_generic_and_type_declaration_type_subst_kind_]

list_post_item_attribute_:
  
    {} [@name nil_post_item_attribute]
| post_item_attribute list_post_item_attribute_
    {} [@name cons_post_item_attribute]

list_signature_element_:
  
    {} [@name nil_signature_element]
| SEMISEMI list_signature_element_
    {} [@name cons_signature_element]
| signature_item list_signature_element_
    {} [@name cons_signature_element]

list_structure_element_:
  
    {} [@name nil_structure_element]
| SEMISEMI list_structure_element_
    {} [@name cons_structure_element]
| SEMISEMI seq_expr list_post_item_attribute_ list_structure_element_
    {} [@name cons_structure_element]
| structure_item list_structure_element_
    {} [@name cons_structure_element]

list_text_csig_class_sig_field__:
  
    {} [@name nil_text_csig_class_sig_field_]
| class_sig_field list_text_csig_class_sig_field__
    {} [@name cons_text_csig_class_sig_field_]

list_text_cstr_class_field__:
  
    {} [@name nil_text_cstr_class_field_]
| class_field list_text_cstr_class_field__
    {} [@name cons_text_cstr_class_field_]

list_text_str_structure_item__:
  
    {} [@name nil_text_str_structure_item_]
| structure_item list_text_str_structure_item__
    {} [@name cons_text_str_structure_item_]

list_use_file_element_:
  
    {} [@name nil_use_file_element]
| SEMISEMI list_use_file_element_
    {} [@name cons_use_file_element]
| SEMISEMI seq_expr list_post_item_attribute_ list_use_file_element_
    {} [@name cons_use_file_element]
| structure_item list_use_file_element_
    {} [@name cons_use_file_element]
| toplevel_directive list_use_file_element_
    {} [@name cons_use_file_element]

nonempty_list_mkrhs_LIDENT__:
  LIDENT
    {} [@name one_mkrhs_LIDENT_]
| LIDENT nonempty_list_mkrhs_LIDENT__
    {} [@name more_mkrhs_LIDENT_]

nonempty_list_modality_:
  LIDENT
    {} [@name one_modality]
| LIDENT nonempty_list_modality_
    {} [@name more_modality]

nonempty_list_mode_:
  LIDENT
    {} [@name one_mode]
| LIDENT nonempty_list_mode_
    {} [@name more_mode]

nonempty_list_mode_legacy_:
  LOCAL
    {} [@name one_mode_legacy]
| UNIQUE
    {} [@name one_mode_legacy]
| ONCE
    {} [@name one_mode_legacy]
| LOCAL nonempty_list_mode_legacy_
    {} [@name more_mode_legacy]
| UNIQUE nonempty_list_mode_legacy_
    {} [@name more_mode_legacy]
| ONCE nonempty_list_mode_legacy_
    {} [@name more_mode_legacy]

nonempty_list_newtype_:
  newtype
    {} [@name one_newtype]
| newtype nonempty_list_newtype_
    {} [@name more_newtype]

nonempty_list_raw_string_:
  STRING
    {} [@name one_raw_string]
| STRING nonempty_list_raw_string_
    {} [@name more_raw_string]

reversed_llist_preceded_CONSTRAINT_constrain__:
  
    {}
| reversed_llist_preceded_CONSTRAINT_constrain__ CONSTRAINT core_type EQUAL core_type
    {}

reversed_nonempty_llist_comprehension_clause_:
  comprehension_clause
    {}
| reversed_nonempty_llist_comprehension_clause_ comprehension_clause
    {}

reversed_nonempty_llist_functor_arg_:
  functor_arg
    {}
| reversed_nonempty_llist_functor_arg_ functor_arg
    {}

reversed_nonempty_llist_labeled_simple_expr_:
  labeled_simple_expr
    {}
| reversed_nonempty_llist_labeled_simple_expr_ labeled_simple_expr
    {}

reversed_nonempty_llist_name_tag_:
  name_tag
    {}
| reversed_nonempty_llist_name_tag_ name_tag
    {}

reversed_nonempty_llist_typevar_:
  QUOTE ident
    {}
| LPAREN QUOTE ident COLON jkind_annotation RPAREN
    {}
| reversed_nonempty_llist_typevar_ QUOTE ident
    {}
| reversed_nonempty_llist_typevar_ LPAREN QUOTE ident COLON jkind_annotation RPAREN
    {}

reversed_nonempty_concat_fun_param_as_list_:
  fun_param_as_list
    {}
| reversed_nonempty_concat_fun_param_as_list_ fun_param_as_list
    {}

reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_:
  alias_type
    {}
| reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ AMPERSAND alias_type
    {}

reversed_separated_nonempty_llist_AND_comprehension_clause_binding_:
  comprehension_clause_binding
    {}
| reversed_separated_nonempty_llist_AND_comprehension_clause_binding_ AND comprehension_clause_binding
    {}

reversed_separated_nonempty_llist_AND_with_constraint_:
  with_constraint
    {}
| reversed_separated_nonempty_llist_AND_with_constraint_ AND with_constraint
    {}

reversed_separated_nonempty_llist_BAR_row_field_:
  row_field
    {}
| reversed_separated_nonempty_llist_BAR_row_field_ BAR row_field
    {}

reversed_separated_nonempty_llist_COMMA_core_type_:
  core_type
    {}
| reversed_separated_nonempty_llist_COMMA_core_type_ COMMA core_type
    {}

reversed_separated_nonempty_llist_COMMA_parenthesized_type_parameter_:
  parenthesized_type_parameter
    {}
| reversed_separated_nonempty_llist_COMMA_parenthesized_type_parameter_ COMMA parenthesized_type_parameter
    {}

reversed_separated_nonempty_llist_COMMA_type_parameter_:
  type_parameter
    {}
| reversed_separated_nonempty_llist_COMMA_type_parameter_ COMMA type_parameter
    {}

reversed_separated_nonempty_llist_STAR_constructor_argument_:
  atomic_type optional_atat_modalities_expr
    {}
| GLOBAL atomic_type optional_atat_modalities_expr
    {}
| reversed_separated_nonempty_llist_STAR_constructor_argument_ STAR atomic_type optional_atat_modalities_expr
    {}
| reversed_separated_nonempty_llist_STAR_constructor_argument_ STAR GLOBAL atomic_type optional_atat_modalities_expr
    {}

reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_:
  atomic_type %prec STAR
    {}
| LIDENT COLON atomic_type %prec STAR
    {}
| reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ STAR atomic_type %prec STAR
    {}
| reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ STAR LIDENT COLON atomic_type %prec STAR
    {}

reversed_separated_nontrivial_llist_COMMA_one_type_parameter_of_several_:
  reversed_separated_nontrivial_llist_COMMA_one_type_parameter_of_several_ COMMA core_type
    {}
| reversed_separated_nontrivial_llist_COMMA_one_type_parameter_of_several_ COMMA QUOTE ident COLON jkind_annotation
    {}
| reversed_separated_nontrivial_llist_COMMA_one_type_parameter_of_several_ COMMA UNDERSCORE COLON jkind_annotation
    {}
| core_type COMMA core_type
    {}
| core_type COMMA QUOTE ident COLON jkind_annotation
    {}
| core_type COMMA UNDERSCORE COLON jkind_annotation
    {}
| QUOTE ident COLON jkind_annotation COMMA core_type
    {}
| QUOTE ident COLON jkind_annotation COMMA QUOTE ident COLON jkind_annotation
    {}
| QUOTE ident COLON jkind_annotation COMMA UNDERSCORE COLON jkind_annotation
    {}
| UNDERSCORE COLON jkind_annotation COMMA core_type
    {}
| UNDERSCORE COLON jkind_annotation COMMA QUOTE ident COLON jkind_annotation
    {}
| UNDERSCORE COLON jkind_annotation COMMA UNDERSCORE COLON jkind_annotation
    {}

separated_or_terminated_nonempty_list_SEMI_expr_:
  fun_expr
    {} [@name none_SEMI]
| fun_expr SEMI
    {} [@name some_SEMI]
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {} [@name none_SEMI]
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ SEMI
    {} [@name some_SEMI]
| STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {} [@name none_SEMI]
| STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ SEMI
    {} [@name some_SEMI]
| fun_expr SEMI separated_or_terminated_nonempty_list_SEMI_expr_
    {}
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ SEMI separated_or_terminated_nonempty_list_SEMI_expr_
    {}
| STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ SEMI separated_or_terminated_nonempty_list_SEMI_expr_
    {}

separated_or_terminated_nonempty_list_SEMI_object_expr_field_:
  LIDENT option_preceded_EQUAL_expr__
    {} [@name none_SEMI]
| LIDENT option_preceded_EQUAL_expr__ SEMI
    {} [@name some_SEMI]
| LIDENT option_preceded_EQUAL_expr__ SEMI separated_or_terminated_nonempty_list_SEMI_object_expr_field_
    {}

separated_or_terminated_nonempty_list_SEMI_pattern_:
  pattern
    {} [@name none_SEMI]
| pattern SEMI
    {} [@name some_SEMI]
| pattern SEMI separated_or_terminated_nonempty_list_SEMI_pattern_
    {}

separated_or_terminated_nonempty_list_SEMI_record_expr_field_:
  label_longident option_type_constraint_ option_preceded_EQUAL_expr__
    {} [@name none_SEMI]
| label_longident option_type_constraint_ option_preceded_EQUAL_expr__ SEMI
    {} [@name some_SEMI]
| label_longident option_type_constraint_ option_preceded_EQUAL_expr__ SEMI separated_or_terminated_nonempty_list_SEMI_record_expr_field_
    {}

reversed_preceded_or_separated_nonempty_llist_BAR_match_case_:
  match_case
    {} [@name none_BAR]
| BAR match_case
    {} [@name some_BAR]
| reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ BAR match_case
    {}

reversed_bar_llist_constructor_declaration_:
  generic_constructor_declaration_epsilon_
    {}
| generic_constructor_declaration_BAR_
    {}
| reversed_bar_llist_constructor_declaration_ generic_constructor_declaration_BAR_
    {}

reversed_bar_llist_extension_constructor_:
  generic_constructor_declaration_epsilon_
    {}
| extension_constructor_rebind_epsilon_
    {}
| generic_constructor_declaration_BAR_
    {}
| extension_constructor_rebind_BAR_
    {}
| reversed_bar_llist_extension_constructor_ generic_constructor_declaration_BAR_
    {}
| reversed_bar_llist_extension_constructor_ extension_constructor_rebind_BAR_
    {}

reversed_bar_llist_extension_constructor_declaration_:
  generic_constructor_declaration_epsilon_
    {}
| generic_constructor_declaration_BAR_
    {}
| reversed_bar_llist_extension_constructor_declaration_ generic_constructor_declaration_BAR_
    {}

listx_SEMI_record_pat_field_UNDERSCORE_:
  label_longident option_preceded_COLON_core_type__ option_preceded_EQUAL_pattern__
    {} [@name none_SEMI]
| label_longident option_preceded_COLON_core_type__ option_preceded_EQUAL_pattern__ SEMI
    {} [@name some_SEMI]
| label_longident option_preceded_COLON_core_type__ option_preceded_EQUAL_pattern__ SEMI UNDERSCORE option_SEMI_
    {}
| label_longident option_preceded_COLON_core_type__ option_preceded_EQUAL_pattern__ SEMI listx_SEMI_record_pat_field_UNDERSCORE_
    {}

implementation:
  structure EOF
    {}

interface:
  signature EOF
    {}

toplevel_phrase:
  seq_expr list_post_item_attribute_ SEMISEMI
    {}
| list_text_str_structure_item__ SEMISEMI
    {}
| toplevel_directive SEMISEMI
    {}
| EOF
    {}

use_file:
  list_use_file_element_ EOF
    {}
| seq_expr list_post_item_attribute_ list_use_file_element_ EOF
    {}

parse_module_type:
  module_type EOF
    {}

parse_module_expr:
  module_expr EOF
    {}

parse_core_type:
  core_type EOF
    {}

parse_expression:
  seq_expr EOF
    {}

parse_pattern:
  pattern EOF
    {}

parse_mty_longident:
  mty_longident EOF
    {}

parse_val_longident:
  val_longident EOF
    {}

parse_constr_longident:
  constr_longident EOF
    {}

parse_mod_ext_longident:
  mod_ext_longident EOF
    {}

parse_mod_longident:
  mod_longident EOF
    {}

parse_any_longident:
  any_longident EOF
    {}

functor_arg:
  LPAREN RPAREN
    {}
| LPAREN module_name COLON module_type RPAREN
    {}
| LPAREN module_name COLON module_type_atomic at_mode_expr RPAREN
    {}

module_name:
  UIDENT
    {}
| UNDERSCORE
    {}

module_name_modal_at_mode_expr_:
  module_name
    {}
| LPAREN module_name at_mode_expr RPAREN
    {}

module_name_modal_atat_modalities_expr_:
  module_name
    {}
| LPAREN module_name atat_modalities_expr RPAREN
    {}

module_expr:
  STRUCT list_attribute_ structure END
    {}
| FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER module_expr
    {}
| paren_module_expr
    {}
| module_expr attribute
    {}
| mod_longident
    {}
| module_expr paren_module_expr
    {}
| module_expr LPAREN RPAREN
    {}
| extension
    {}

paren_module_expr:
  LPAREN module_expr COLON module_type RPAREN
    {}
| LPAREN module_expr COLON module_type_atomic at_mode_expr RPAREN
    {}
| LPAREN module_expr at_mode_expr RPAREN
    {}
| LPAREN module_expr RPAREN
    {}
| LPAREN VAL list_attribute_ fun_expr RPAREN
    {}
| LPAREN VAL list_attribute_ FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ RPAREN
    {}
| LPAREN VAL list_attribute_ STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ RPAREN
    {}
| LPAREN VAL list_attribute_ fun_expr COLON module_type RPAREN
    {}
| LPAREN VAL list_attribute_ FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COLON module_type RPAREN
    {}
| LPAREN VAL list_attribute_ STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COLON module_type RPAREN
    {}
| LPAREN VAL list_attribute_ fun_expr COLON module_type COLONGREATER module_type RPAREN
    {}
| LPAREN VAL list_attribute_ FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COLON module_type COLONGREATER module_type RPAREN
    {}
| LPAREN VAL list_attribute_ STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COLON module_type COLONGREATER module_type RPAREN
    {}
| LPAREN VAL list_attribute_ fun_expr COLONGREATER module_type RPAREN
    {}
| LPAREN VAL list_attribute_ FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COLONGREATER module_type RPAREN
    {}
| LPAREN VAL list_attribute_ STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COLONGREATER module_type RPAREN
    {}

structure:
  list_structure_element_
    {}
| seq_expr list_post_item_attribute_ list_structure_element_
    {}

structure_item:
  let_bindings_ext_
    {}
| item_extension list_post_item_attribute_
    {}
| floating_attribute
    {}
| kind_abbreviation_decl
    {}
| primitive_declaration
    {}
| value_description
    {}
| generic_type_declaration_nonrec_flag_type_kind_ list_generic_and_type_declaration_type_kind__
    {}
| TYPE ext list_attribute_ type_parameters type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_ list_post_item_attribute_
    {}
| TYPE ext list_attribute_ NONREC type_parameters type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_ list_post_item_attribute_
    {}
| str_exception_declaration
    {}
| MODULE ext list_attribute_ module_name_modal_at_mode_expr_ module_binding_body list_post_item_attribute_
    {}
| MODULE ext list_attribute_ REC module_name_modal_at_mode_expr_ module_binding_body list_post_item_attribute_ list_and_module_binding_
    {}
| module_type_declaration
    {}
| open_declaration
    {}
| CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT class_fun_binding list_post_item_attribute_ list_and_class_declaration_
    {}
| class_type_declarations
    {}
| include_kind ext list_attribute_ module_expr list_post_item_attribute_
    {}

module_binding_body:
  EQUAL module_expr
    {}
| COLON module_type EQUAL module_expr
    {}
| COLON module_type_atomic at_mode_expr EQUAL module_expr
    {}
| at_mode_expr EQUAL module_expr
    {}
| functor_arg module_binding_body
    {}

include_kind:
  INCLUDE %prec below_FUNCTOR
    {}
| INCLUDE FUNCTOR
    {}

module_type_declaration:
  MODULE TYPE ext list_attribute_ ident option_preceded_EQUAL_module_type__ list_post_item_attribute_
    {}

open_declaration:
  OPEN ext list_attribute_ module_expr list_post_item_attribute_
    {}
| OPEN BANG ext list_attribute_ module_expr list_post_item_attribute_
    {}

open_description:
  OPEN ext list_attribute_ mod_ext_longident list_post_item_attribute_
    {}
| OPEN BANG ext list_attribute_ mod_ext_longident list_post_item_attribute_
    {}

module_type_atomic:
  SIG list_attribute_ signature END
    {}
| LPAREN module_type RPAREN
    {}
| mty_longident
    {}

module_type:
  module_type_atomic
    {}
| FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type %prec below_WITH
    {}
| FUNCTOR list_attribute_ reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type_atomic at_mode_expr %prec below_WITH
    {}
| reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type %prec below_WITH
    {}
| reversed_nonempty_llist_functor_arg_ MINUSGREATER module_type_atomic at_mode_expr %prec below_WITH
    {}
| MODULE TYPE OF list_attribute_ module_expr %prec below_LBRACKETAT
    {}
| module_type attribute
    {}
| module_type MINUSGREATER module_type %prec below_WITH
    {}
| module_type MINUSGREATER module_type_atomic at_mode_expr %prec below_WITH
    {}
| module_type_atomic at_mode_expr MINUSGREATER module_type %prec below_WITH
    {}
| module_type_atomic at_mode_expr MINUSGREATER module_type_atomic at_mode_expr %prec below_WITH
    {}
| module_type WITH reversed_separated_nonempty_llist_AND_with_constraint_
    {}
| extension
    {}
| module_type WITH mod_ext_longident
    {}

signature:
  optional_atat_modalities_expr list_signature_element_
    {}

signature_item:
  item_extension list_post_item_attribute_
    {}
| floating_attribute
    {}
| kind_abbreviation_decl
    {}
| value_description
    {}
| primitive_declaration
    {}
| generic_type_declaration_nonrec_flag_type_kind_ list_generic_and_type_declaration_type_kind__
    {}
| generic_type_declaration_no_nonrec_flag_type_subst_kind_ list_generic_and_type_declaration_type_subst_kind__
    {}
| TYPE ext list_attribute_ type_parameters type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
    {}
| TYPE ext list_attribute_ NONREC type_parameters type_longident PLUSEQ private_flag reversed_bar_llist_extension_constructor_declaration_ list_post_item_attribute_
    {}
| sig_exception_declaration
    {}
| MODULE ext list_attribute_ module_name_modal_atat_modalities_expr_ module_declaration_body___anonymous_8_ list_post_item_attribute_
    {}
| MODULE ext list_attribute_ module_name_modal_atat_modalities_expr_ EQUAL mod_longident optional_atat_modalities_expr list_post_item_attribute_
    {}
| module_subst
    {}
| MODULE ext list_attribute_ REC module_name COLON module_type optional_atat_modalities_expr list_post_item_attribute_ list_and_module_declaration_
    {}
| module_type_declaration
    {}
| module_type_subst
    {}
| open_description
    {}
| CLASS ext list_attribute_ virtual_flag formal_class_parameters LIDENT COLON class_type list_post_item_attribute_ list_and_class_description_
    {}
| class_type_declarations
    {}
| include_kind ext list_attribute_ module_type list_post_item_attribute_ optional_atat_modalities_expr
    {}

module_declaration_body___anonymous_8_:
  COLON module_type optional_atat_modalities_expr
    {}
| functor_arg module_declaration_body_module_type_with_optional_modes_
    {}

module_declaration_body_module_type_with_optional_modes_:
  COLON module_type
    {}
| COLON module_type_atomic at_mode_expr
    {}
| functor_arg module_declaration_body_module_type_with_optional_modes_
    {}

module_subst:
  MODULE ext list_attribute_ UIDENT COLONEQUAL mod_ext_longident list_post_item_attribute_
    {}

module_type_subst:
  MODULE TYPE ext list_attribute_ ident COLONEQUAL module_type list_post_item_attribute_
    {}

class_fun_binding:
  EQUAL class_expr
    {}
| COLON class_type EQUAL class_expr
    {}
| labeled_simple_pattern class_fun_binding
    {}

formal_class_parameters:
  
    {}
| LBRACKET reversed_separated_nonempty_llist_COMMA_type_parameter_ RBRACKET
    {}

class_expr:
  class_simple_expr
    {}
| FUN list_attribute_ class_fun_def
    {}
| let_bindings_no_ext_ IN class_expr
    {}
| LET OPEN list_attribute_ mod_longident IN class_expr
    {}
| LET OPEN BANG list_attribute_ mod_longident IN class_expr
    {}
| class_expr attribute
    {}
| class_simple_expr reversed_nonempty_llist_labeled_simple_expr_
    {}
| extension
    {}

class_simple_expr:
  LPAREN class_expr RPAREN
    {}
| class_longident
    {}
| LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET class_longident
    {}
| LPAREN class_expr COLON class_type RPAREN
    {}
| OBJECT list_attribute_ class_self_pattern list_text_cstr_class_field__ END
    {}

class_fun_def:
  labeled_simple_pattern MINUSGREATER class_expr
    {}
| labeled_simple_pattern class_fun_def
    {}

class_self_pattern:
  LPAREN pattern RPAREN
    {}
| LPAREN pattern COLON core_type RPAREN
    {}
| 
    {}

class_field:
  INHERIT list_attribute_ class_expr option_preceded_AS_mkrhs_LIDENT___ list_post_item_attribute_
    {}
| INHERIT BANG list_attribute_ class_expr option_preceded_AS_mkrhs_LIDENT___ list_post_item_attribute_
    {}
| VAL value list_post_item_attribute_
    {}
| METHOD method_ list_post_item_attribute_
    {}
| CONSTRAINT list_attribute_ constrain_field list_post_item_attribute_
    {}
| INITIALIZER list_attribute_ seq_expr list_post_item_attribute_
    {}
| item_extension list_post_item_attribute_
    {}
| floating_attribute
    {}

value:
  list_attribute_ virtual_with_mutable_flag LIDENT COLON core_type
    {}
| list_attribute_ mutable_flag LIDENT EQUAL seq_expr
    {}
| BANG list_attribute_ mutable_flag LIDENT EQUAL seq_expr
    {}
| list_attribute_ mutable_flag LIDENT type_constraint EQUAL seq_expr
    {}
| BANG list_attribute_ mutable_flag LIDENT type_constraint EQUAL seq_expr
    {}

method_:
  list_attribute_ virtual_with_private_flag LIDENT COLON possibly_poly_core_type_
    {}
| list_attribute_ private_flag LIDENT strict_binding_modes
    {}
| BANG list_attribute_ private_flag LIDENT strict_binding_modes
    {}
| list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ EQUAL seq_expr
    {}
| BANG list_attribute_ private_flag LIDENT COLON possibly_poly_core_type_ EQUAL seq_expr
    {}
| list_attribute_ private_flag LIDENT COLON TYPE newtypes DOT core_type EQUAL seq_expr
    {}
| BANG list_attribute_ private_flag LIDENT COLON TYPE newtypes DOT core_type EQUAL seq_expr
    {}

class_type:
  class_signature
    {}
| optlabel tuple_type MINUSGREATER class_type
    {}
| LIDENT COLON tuple_type MINUSGREATER class_type
    {}
| tuple_type MINUSGREATER class_type
    {}

class_signature:
  clty_longident
    {}
| LBRACKET reversed_separated_nonempty_llist_COMMA_core_type_ RBRACKET clty_longident
    {}
| extension
    {}
| OBJECT list_attribute_ class_self_type list_text_csig_class_sig_field__ END
    {}
| class_signature attribute
    {}
| LET OPEN list_attribute_ mod_longident IN class_signature
    {}
| LET OPEN BANG list_attribute_ mod_longident IN class_signature
    {}

class_self_type:
  LPAREN core_type RPAREN
    {}
| 
    {}

class_sig_field:
  INHERIT list_attribute_ class_signature list_post_item_attribute_
    {}
| VAL list_attribute_ mutable_virtual_flags LIDENT COLON core_type list_post_item_attribute_
    {}
| METHOD list_attribute_ private_virtual_flags LIDENT COLON possibly_poly_core_type_ list_post_item_attribute_
    {}
| CONSTRAINT list_attribute_ constrain_field list_post_item_attribute_
    {}
| item_extension list_post_item_attribute_
    {}
| floating_attribute
    {}

constrain_field:
  core_type EQUAL core_type
    {}

class_type_declarations:
  CLASS TYPE ext list_attribute_ virtual_flag formal_class_parameters LIDENT EQUAL class_signature list_post_item_attribute_ list_and_class_type_declaration_
    {}

fun_seq_expr:
  fun_expr %prec below_SEMI
    {}
| fun_expr SEMI
    {}
| fun_expr SEMI seq_expr
    {}
| fun_expr SEMI PERCENT attr_id seq_expr
    {}

seq_expr:
  fun_seq_expr
    {}
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}

labeled_simple_pattern:
  QUESTION LPAREN label_let_pattern option_preceded_EQUAL_seq_expr__ RPAREN
    {}
| QUESTION LIDENT
    {}
| OPTLABEL LPAREN let_pattern option_preceded_EQUAL_seq_expr__ RPAREN
    {}
| OPTLABEL pattern_var
    {}
| TILDE LPAREN label_let_pattern RPAREN
    {}
| TILDE LIDENT
    {}
| LABEL simple_pattern_extend_modes_or_poly
    {}
| simple_pattern_extend_modes_or_poly
    {}

pattern_var:
  LIDENT
    {}
| UNDERSCORE
    {}

optional_poly_type_and_modes:
  
    {}
| at_mode_expr
    {}
| COLON tuple_type at_mode_expr
    {}
| COLON reversed_nonempty_llist_typevar_ DOT tuple_type at_mode_expr
    {}
| COLON possibly_poly_core_type_
    {}

label_let_pattern:
  LIDENT optional_poly_type_and_modes
    {}
| nonempty_list_mode_legacy_ LIDENT optional_poly_type_and_modes
    {}

let_pattern:
  pattern optional_poly_type_and_modes
    {}
| nonempty_list_mode_legacy_ pattern optional_poly_type_and_modes
    {}

simple_pattern_extend_modes_or_poly:
  simple_pattern
    {}
| LPAREN pattern_with_modes_or_poly RPAREN
    {}

pattern_with_modes_or_poly:
  nonempty_list_mode_legacy_ pattern optional_poly_type_and_modes
    {}
| pattern COLON tuple_type at_mode_expr
    {}
| pattern COLON reversed_nonempty_llist_typevar_ DOT tuple_type at_mode_expr
    {}
| pattern at_mode_expr
    {}
| pattern COLON reversed_nonempty_llist_typevar_ DOT core_type
    {}

optional_atomic_constraint_:
  COLON atomic_type
    {}
| at_mode_expr
    {}
| 
    {}

fun_:
  FUN ext list_attribute_ fun_params optional_atomic_constraint_ MINUSGREATER fun_body
    {}
| STACK FUN ext list_attribute_ fun_params optional_atomic_constraint_ MINUSGREATER fun_body
    {}

fun_expr:
  simple_expr %prec below_HASH
    {}
| LET MODULE ext list_attribute_ module_name_modal_at_mode_expr_ module_binding_body IN seq_expr
    {}
| LET EXCEPTION ext list_attribute_ constr_ident generalized_constructor_arguments list_attribute_ IN seq_expr
    {}
| LET OPEN ext list_attribute_ module_expr IN seq_expr
    {}
| LET OPEN BANG ext list_attribute_ module_expr IN seq_expr
    {}
| MATCH ext list_attribute_ seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| TRY ext list_attribute_ seq_expr WITH reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| OVERWRITE ext list_attribute_ seq_expr WITH fun_expr
    {}
| OVERWRITE ext list_attribute_ seq_expr WITH FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| OVERWRITE ext list_attribute_ seq_expr WITH STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| IF ext list_attribute_ seq_expr THEN fun_expr ELSE fun_expr
    {}
| IF ext list_attribute_ seq_expr THEN fun_expr ELSE FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| IF ext list_attribute_ seq_expr THEN fun_expr ELSE STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE fun_expr
    {}
| IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| IF ext list_attribute_ seq_expr THEN STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE fun_expr
    {}
| IF ext list_attribute_ seq_expr THEN STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| IF ext list_attribute_ seq_expr THEN STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ ELSE STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| IF ext list_attribute_ seq_expr THEN fun_expr
    {}
| IF ext list_attribute_ seq_expr THEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| IF ext list_attribute_ seq_expr THEN STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| WHILE ext list_attribute_ seq_expr DO seq_expr DONE
    {}
| FOR ext list_attribute_ pattern EQUAL seq_expr direction_flag seq_expr DO seq_expr DONE
    {}
| ASSERT ext list_attribute_ simple_expr %prec below_HASH
    {}
| LAZY ext list_attribute_ simple_expr %prec below_HASH
    {}
| subtractive fun_expr %prec prec_unary_minus
    {}
| subtractive FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ %prec prec_unary_minus
    {}
| subtractive STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ %prec prec_unary_minus
    {}
| additive fun_expr %prec prec_unary_plus
    {}
| additive FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ %prec prec_unary_plus
    {}
| additive STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ %prec prec_unary_plus
    {}
| fun_
    {}
| simple_expr reversed_nonempty_llist_labeled_simple_expr_
    {}
| STACK simple_expr %prec below_HASH
    {}
| reversed_labeled_tuple_body %prec below_COMMA
    {}
| constr_longident simple_expr %prec below_HASH
    {}
| STACK constr_longident simple_expr %prec below_HASH
    {}
| name_tag simple_expr %prec below_HASH
    {}
| fun_expr INFIXOP0 fun_expr
    {}
| fun_expr INFIXOP0 FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr INFIXOP0 STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr AT fun_expr
    {}
| fun_expr AT FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr AT STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr ATAT fun_expr
    {}
| fun_expr ATAT FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr ATAT STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr INFIXOP1 fun_expr
    {}
| fun_expr INFIXOP1 FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr INFIXOP1 STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr INFIXOP2 fun_expr
    {}
| fun_expr INFIXOP2 FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr INFIXOP2 STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr INFIXOP3 fun_expr
    {}
| fun_expr INFIXOP3 FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr INFIXOP3 STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr MOD fun_expr
    {}
| fun_expr MOD FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr MOD STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr INFIXOP4 fun_expr
    {}
| fun_expr INFIXOP4 FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr INFIXOP4 STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr PLUS fun_expr
    {}
| fun_expr PLUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr PLUS STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr PLUSDOT fun_expr
    {}
| fun_expr PLUSDOT FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr PLUSDOT STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr PLUSEQ fun_expr
    {}
| fun_expr PLUSEQ FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr PLUSEQ STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr MINUS fun_expr
    {}
| fun_expr MINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr MINUS STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr MINUSDOT fun_expr
    {}
| fun_expr MINUSDOT FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr MINUSDOT STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr STAR fun_expr
    {}
| fun_expr STAR FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr STAR STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr PERCENT fun_expr
    {}
| fun_expr PERCENT FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr PERCENT STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr EQUAL fun_expr
    {}
| fun_expr EQUAL FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr EQUAL STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr LESS fun_expr
    {}
| fun_expr LESS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr LESS STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr GREATER fun_expr
    {}
| fun_expr GREATER FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr GREATER STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr OR fun_expr
    {}
| fun_expr OR FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr OR STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr BARBAR fun_expr
    {}
| fun_expr BARBAR FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr BARBAR STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr AMPERSAND fun_expr
    {}
| fun_expr AMPERSAND FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr AMPERSAND STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr AMPERAMPER fun_expr
    {}
| fun_expr AMPERAMPER FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr AMPERAMPER STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr COLONEQUAL fun_expr
    {}
| fun_expr COLONEQUAL FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr COLONEQUAL STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| let_bindings_ext_ IN seq_expr
    {}
| LETOP letop_bindings IN seq_expr
    {}
| fun_expr COLONCOLON fun_expr
    {}
| fun_expr COLONCOLON FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr COLONCOLON STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| LIDENT LESSMINUS fun_expr
    {}
| LIDENT LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| LIDENT LESSMINUS STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| simple_expr DOT label_longident LESSMINUS fun_expr
    {}
| simple_expr DOT label_longident LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| simple_expr DOT label_longident LESSMINUS STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS fun_expr
    {}
| simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| simple_expr DOT LPAREN seq_expr RPAREN LESSMINUS STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| simple_expr DOT LBRACE seq_expr RBRACE LESSMINUS fun_expr
    {}
| simple_expr DOT LBRACE seq_expr RBRACE LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| simple_expr DOT LBRACE seq_expr RBRACE LESSMINUS STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS fun_expr
    {}
| simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| simple_expr DOT LBRACKET seq_expr RBRACKET LESSMINUS STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS fun_expr
    {} [@name none___anonymous_22]
| simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {} [@name none___anonymous_22]
| simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {} [@name none___anonymous_22]
| simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS fun_expr
    {} [@name some___anonymous_22]
| simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {} [@name some___anonymous_22]
| simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN LESSMINUS STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {} [@name some___anonymous_22]
| simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS fun_expr
    {} [@name none___anonymous_22]
| simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {} [@name none___anonymous_22]
| simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {} [@name none___anonymous_22]
| simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS fun_expr
    {} [@name some___anonymous_22]
| simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {} [@name some___anonymous_22]
| simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE LESSMINUS STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {} [@name some___anonymous_22]
| simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS fun_expr
    {} [@name none___anonymous_22]
| simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {} [@name none___anonymous_22]
| simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {} [@name none___anonymous_22]
| simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS fun_expr
    {} [@name some___anonymous_22]
| simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {} [@name some___anonymous_22]
| simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET LESSMINUS STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {} [@name some___anonymous_22]
| fun_expr attribute
    {}
| UNDERSCORE
    {}
| LOCAL seq_expr
    {}
| UNIQUE seq_expr
    {}
| ONCE seq_expr
    {}
| EXCLAVE seq_expr
    {}

simple_expr:
  LPAREN seq_expr RPAREN
    {}
| LPAREN seq_expr COLON core_type RPAREN
    {}
| LPAREN seq_expr COLON tuple_type at_mode_expr RPAREN
    {}
| LPAREN seq_expr COLON core_type COLONGREATER core_type RPAREN
    {}
| LPAREN seq_expr COLON core_type COLONGREATER tuple_type at_mode_expr RPAREN
    {}
| LPAREN seq_expr COLONGREATER core_type RPAREN
    {}
| LPAREN seq_expr COLONGREATER tuple_type at_mode_expr RPAREN
    {}
| simple_expr DOT LPAREN seq_expr RPAREN
    {}
| simple_expr DOT LBRACE seq_expr RBRACE
    {}
| simple_expr DOT LBRACKET seq_expr RBRACKET
    {}
| simple_expr DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
    {} [@name none___anonymous_22]
| simple_expr DOT mod_longident DOTOP LPAREN separated_or_terminated_nonempty_list_SEMI_expr_ RPAREN
    {} [@name some___anonymous_22]
| simple_expr DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
    {} [@name none___anonymous_22]
| simple_expr DOT mod_longident DOTOP LBRACE separated_or_terminated_nonempty_list_SEMI_expr_ RBRACE
    {} [@name some___anonymous_22]
| simple_expr DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
    {} [@name none___anonymous_22]
| simple_expr DOT mod_longident DOTOP LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
    {} [@name some___anonymous_22]
| BEGIN ext list_attribute_ seq_expr END
    {}
| BEGIN ext list_attribute_ END
    {}
| NEW ext list_attribute_ class_longident
    {}
| LPAREN MODULE ext list_attribute_ module_expr RPAREN
    {}
| LPAREN MODULE ext list_attribute_ module_expr COLON module_type RPAREN
    {}
| OBJECT ext list_attribute_ class_self_pattern list_text_cstr_class_field__ END
    {}
| val_longident
    {}
| constr_longident %prec prec_constant_constructor
    {}
| name_tag %prec prec_constant_constructor
    {}
| PREFIXOP simple_expr
    {}
| BANG simple_expr
    {}
| LBRACELESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ GREATERRBRACE
    {}
| LBRACELESS GREATERRBRACE
    {}
| simple_expr DOT label_longident
    {}
| simple_expr DOTHASH label_longident
    {}
| mod_longident DOT LPAREN seq_expr RPAREN
    {}
| mod_longident DOT LBRACELESS separated_or_terminated_nonempty_list_SEMI_object_expr_field_ GREATERRBRACE
    {}
| simple_expr HASH LIDENT
    {}
| simple_expr HASH_SUFFIX LIDENT
    {}
| simple_expr HASHOP simple_expr
    {}
| extension
    {}
| mod_longident DOT LPAREN RPAREN
    {}
| LBRACE record_expr_content RBRACE
    {}
| HASHLBRACE record_expr_content RBRACE
    {}
| mod_longident DOT LBRACE record_expr_content RBRACE
    {}
| LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_expr_ BARRBRACKET
    {}
| LBRACKETBAR BARRBRACKET
    {}
| mod_longident DOT LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_expr_ BARRBRACKET
    {}
| mod_longident DOT LBRACKETBAR BARRBRACKET
    {}
| LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
    {}
| mod_longident DOT LBRACKET fun_expr reversed_nonempty_llist_comprehension_clause_ RBRACKET
    {}
| mod_longident DOT LBRACKET FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ reversed_nonempty_llist_comprehension_clause_ RBRACKET
    {}
| mod_longident DOT LBRACKET STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ reversed_nonempty_llist_comprehension_clause_ RBRACKET
    {}
| mod_longident DOT LBRACKETBAR fun_expr reversed_nonempty_llist_comprehension_clause_ BARRBRACKET
    {}
| mod_longident DOT LBRACKETBAR FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ reversed_nonempty_llist_comprehension_clause_ BARRBRACKET
    {}
| mod_longident DOT LBRACKETBAR STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ reversed_nonempty_llist_comprehension_clause_ BARRBRACKET
    {}
| mod_longident DOT LBRACKETCOLON fun_expr reversed_nonempty_llist_comprehension_clause_ COLONRBRACKET
    {}
| mod_longident DOT LBRACKETCOLON FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ reversed_nonempty_llist_comprehension_clause_ COLONRBRACKET
    {}
| mod_longident DOT LBRACKETCOLON STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ reversed_nonempty_llist_comprehension_clause_ COLONRBRACKET
    {}
| mod_longident DOT LBRACKET separated_or_terminated_nonempty_list_SEMI_expr_ RBRACKET
    {}
| mod_longident DOT LBRACKET RBRACKET
    {}
| mod_longident DOT LPAREN MODULE ext list_attribute_ module_expr COLON module_type RPAREN
    {}
| HASHLPAREN reversed_labeled_tuple_body RPAREN
    {}
| LBRACKETCOLON separated_or_terminated_nonempty_list_SEMI_expr_ COLONRBRACKET
    {}
| LBRACKETCOLON COLONRBRACKET
    {}
| mod_longident DOT LBRACKETCOLON separated_or_terminated_nonempty_list_SEMI_expr_ COLONRBRACKET
    {}
| mod_longident DOT LBRACKETCOLON COLONRBRACKET
    {}
| constant
    {}
| LBRACKET fun_expr reversed_nonempty_llist_comprehension_clause_ RBRACKET
    {}
| LBRACKET FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ reversed_nonempty_llist_comprehension_clause_ RBRACKET
    {}
| LBRACKET STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ reversed_nonempty_llist_comprehension_clause_ RBRACKET
    {}
| LBRACKETBAR fun_expr reversed_nonempty_llist_comprehension_clause_ BARRBRACKET
    {}
| LBRACKETBAR FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ reversed_nonempty_llist_comprehension_clause_ BARRBRACKET
    {}
| LBRACKETBAR STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ reversed_nonempty_llist_comprehension_clause_ BARRBRACKET
    {}
| LBRACKETCOLON fun_expr reversed_nonempty_llist_comprehension_clause_ COLONRBRACKET
    {}
| LBRACKETCOLON FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ reversed_nonempty_llist_comprehension_clause_ COLONRBRACKET
    {}
| LBRACKETCOLON STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ reversed_nonempty_llist_comprehension_clause_ COLONRBRACKET
    {}

comprehension_iterator:
  EQUAL fun_expr direction_flag fun_expr
    {}
| EQUAL fun_expr direction_flag FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| EQUAL fun_expr direction_flag STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| EQUAL FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ direction_flag fun_expr
    {}
| EQUAL FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ direction_flag FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| EQUAL FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ direction_flag STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| EQUAL STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ direction_flag fun_expr
    {}
| EQUAL STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ direction_flag FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| EQUAL STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ direction_flag STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| IN fun_expr
    {}
| IN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| IN STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}

comprehension_clause_binding:
  list_attribute_ pattern comprehension_iterator
    {}
| list_attribute_ LOCAL pattern IN fun_expr
    {}
| list_attribute_ LOCAL pattern IN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| list_attribute_ LOCAL pattern IN STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| list_attribute_ UNIQUE pattern IN fun_expr
    {}
| list_attribute_ UNIQUE pattern IN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| list_attribute_ UNIQUE pattern IN STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| list_attribute_ ONCE pattern IN fun_expr
    {}
| list_attribute_ ONCE pattern IN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| list_attribute_ ONCE pattern IN STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}

comprehension_clause:
  FOR reversed_separated_nonempty_llist_AND_comprehension_clause_binding_
    {}
| WHEN fun_expr
    {}
| WHEN FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| WHEN STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}

labeled_simple_expr:
  simple_expr %prec below_HASH
    {}
| LABEL simple_expr %prec below_HASH
    {}
| TILDE LIDENT
    {}
| TILDE LPAREN LIDENT type_constraint RPAREN
    {}
| QUESTION LIDENT
    {}
| OPTLABEL simple_expr %prec below_HASH
    {}

let_binding_body_no_punning:
  val_ident strict_binding_modes
    {}
| nonempty_list_mode_legacy_ val_ident strict_binding_modes
    {}
| LPAREN val_ident at_mode_expr RPAREN strict_binding_modes
    {}
| val_ident COLON core_type EQUAL seq_expr
    {}
| val_ident COLON tuple_type at_mode_expr EQUAL seq_expr
    {}
| val_ident COLON core_type COLONGREATER core_type EQUAL seq_expr
    {}
| val_ident COLON core_type COLONGREATER tuple_type at_mode_expr EQUAL seq_expr
    {}
| val_ident COLONGREATER core_type EQUAL seq_expr
    {}
| val_ident COLONGREATER tuple_type at_mode_expr EQUAL seq_expr
    {}
| val_ident at_mode_expr EQUAL seq_expr
    {}
| nonempty_list_mode_legacy_ val_ident COLON core_type EQUAL seq_expr
    {}
| nonempty_list_mode_legacy_ val_ident COLON tuple_type at_mode_expr EQUAL seq_expr
    {}
| nonempty_list_mode_legacy_ val_ident COLON core_type COLONGREATER core_type EQUAL seq_expr
    {}
| nonempty_list_mode_legacy_ val_ident COLON core_type COLONGREATER tuple_type at_mode_expr EQUAL seq_expr
    {}
| nonempty_list_mode_legacy_ val_ident COLONGREATER core_type EQUAL seq_expr
    {}
| nonempty_list_mode_legacy_ val_ident COLONGREATER tuple_type at_mode_expr EQUAL seq_expr
    {}
| nonempty_list_mode_legacy_ val_ident at_mode_expr EQUAL seq_expr
    {}
| LPAREN val_ident at_mode_expr RPAREN COLON core_type EQUAL seq_expr
    {}
| LPAREN val_ident at_mode_expr RPAREN COLON tuple_type at_mode_expr EQUAL seq_expr
    {}
| LPAREN val_ident at_mode_expr RPAREN COLON core_type COLONGREATER core_type EQUAL seq_expr
    {}
| LPAREN val_ident at_mode_expr RPAREN COLON core_type COLONGREATER tuple_type at_mode_expr EQUAL seq_expr
    {}
| LPAREN val_ident at_mode_expr RPAREN COLONGREATER core_type EQUAL seq_expr
    {}
| LPAREN val_ident at_mode_expr RPAREN COLONGREATER tuple_type at_mode_expr EQUAL seq_expr
    {}
| LPAREN val_ident at_mode_expr RPAREN at_mode_expr EQUAL seq_expr
    {}
| val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type EQUAL seq_expr
    {}
| val_ident COLON reversed_nonempty_llist_typevar_ DOT tuple_type at_mode_expr EQUAL seq_expr
    {}
| nonempty_list_mode_legacy_ val_ident COLON reversed_nonempty_llist_typevar_ DOT core_type EQUAL seq_expr
    {}
| nonempty_list_mode_legacy_ val_ident COLON reversed_nonempty_llist_typevar_ DOT tuple_type at_mode_expr EQUAL seq_expr
    {}
| LPAREN val_ident at_mode_expr RPAREN COLON reversed_nonempty_llist_typevar_ DOT core_type EQUAL seq_expr
    {}
| LPAREN val_ident at_mode_expr RPAREN COLON reversed_nonempty_llist_typevar_ DOT tuple_type at_mode_expr EQUAL seq_expr
    {}
| val_ident COLON TYPE newtypes DOT core_type EQUAL seq_expr
    {}
| nonempty_list_mode_legacy_ val_ident COLON TYPE newtypes DOT core_type EQUAL seq_expr
    {}
| LPAREN val_ident at_mode_expr RPAREN COLON TYPE newtypes DOT core_type EQUAL seq_expr
    {}
| val_ident COLON TYPE newtypes DOT tuple_type at_mode_expr EQUAL seq_expr
    {}
| nonempty_list_mode_legacy_ val_ident COLON TYPE newtypes DOT tuple_type at_mode_expr EQUAL seq_expr
    {}
| LPAREN val_ident at_mode_expr RPAREN COLON TYPE newtypes DOT tuple_type at_mode_expr EQUAL seq_expr
    {}
| pattern_no_exn EQUAL seq_expr
    {}
| simple_pattern_not_ident at_mode_expr EQUAL seq_expr
    {}
| simple_pattern_not_ident COLON core_type EQUAL seq_expr
    {}
| simple_pattern_not_ident COLON tuple_type at_mode_expr EQUAL seq_expr
    {}

let_binding_body:
  let_binding_body_no_punning
    {}
(*| val_ident %prec below_HASH
    {}*)

let_bindings_ext_:
  LET ext list_attribute_ rec_flag let_binding_body list_post_item_attribute_
    {}
| let_bindings_ext_ and_let_binding
    {}

let_bindings_no_ext_:
  LET list_attribute_ rec_flag let_binding_body list_post_item_attribute_
    {}
| LET PERCENT attr_id list_attribute_ rec_flag let_binding_body list_post_item_attribute_
    {}
| let_bindings_no_ext_ and_let_binding
    {}

and_let_binding:
  AND list_attribute_ let_binding_body list_post_item_attribute_
    {}

letop_binding_body:
  val_ident strict_binding_modes
    {}
| val_ident
    {}
| simple_pattern COLON core_type EQUAL seq_expr
    {}
| pattern_no_exn EQUAL seq_expr
    {}

letop_bindings:
  letop_binding_body
    {}
| letop_bindings ANDOP letop_binding_body
    {}

strict_binding_modes:
  EQUAL seq_expr
    {}
| fun_params option_constraint__ EQUAL fun_body
    {}

fun_body:
  FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_seq_expr
    {}

match_case:
  pattern MINUSGREATER seq_expr
    {}
| pattern WHEN seq_expr MINUSGREATER seq_expr
    {}
| pattern MINUSGREATER DOT
    {}

fun_param_as_list:
  LPAREN TYPE newtypes RPAREN
    {}
| LPAREN TYPE LIDENT COLON jkind_annotation RPAREN
    {}
| labeled_simple_pattern
    {}

fun_params:
  reversed_nonempty_concat_fun_param_as_list_
    {}

reversed_labeled_tuple_body:
  reversed_labeled_tuple_body COMMA fun_expr
    {}
| reversed_labeled_tuple_body COMMA FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| reversed_labeled_tuple_body COMMA STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| reversed_labeled_tuple_body COMMA LABEL simple_expr %prec below_HASH
    {}
| reversed_labeled_tuple_body COMMA TILDE LIDENT
    {}
| reversed_labeled_tuple_body COMMA TILDE LPAREN LIDENT type_constraint RPAREN %prec below_HASH
    {}
| fun_expr COMMA fun_expr
    {}
| fun_expr COMMA FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr COMMA STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| fun_expr COMMA LABEL simple_expr %prec below_HASH
    {}
| fun_expr COMMA TILDE LIDENT
    {}
| fun_expr COMMA TILDE LPAREN LIDENT type_constraint RPAREN %prec below_HASH
    {}
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA fun_expr
    {}
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA LABEL simple_expr %prec below_HASH
    {}
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA TILDE LIDENT
    {}
| FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA TILDE LPAREN LIDENT type_constraint RPAREN %prec below_HASH
    {}
| STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA fun_expr
    {}
| STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA LABEL simple_expr %prec below_HASH
    {}
| STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA TILDE LIDENT
    {}
| STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_ COMMA TILDE LPAREN LIDENT type_constraint RPAREN %prec below_HASH
    {}
| LABEL simple_expr COMMA fun_expr
    {}
| LABEL simple_expr COMMA FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| LABEL simple_expr COMMA STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| LABEL simple_expr COMMA LABEL simple_expr %prec below_HASH
    {}
| LABEL simple_expr COMMA TILDE LIDENT
    {}
| LABEL simple_expr COMMA TILDE LPAREN LIDENT type_constraint RPAREN %prec below_HASH
    {}
| TILDE LIDENT COMMA fun_expr
    {}
| TILDE LIDENT COMMA FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| TILDE LIDENT COMMA STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| TILDE LIDENT COMMA LABEL simple_expr %prec below_HASH
    {}
| TILDE LIDENT COMMA TILDE LIDENT
    {}
| TILDE LIDENT COMMA TILDE LPAREN LIDENT type_constraint RPAREN %prec below_HASH
    {}
| TILDE LPAREN LIDENT type_constraint RPAREN COMMA fun_expr
    {}
| TILDE LPAREN LIDENT type_constraint RPAREN COMMA FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| TILDE LPAREN LIDENT type_constraint RPAREN COMMA STACK FUNCTION ext list_attribute_ reversed_preceded_or_separated_nonempty_llist_BAR_match_case_
    {}
| TILDE LPAREN LIDENT type_constraint RPAREN COMMA LABEL simple_expr %prec below_HASH
    {}
| TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LIDENT
    {}
| TILDE LPAREN LIDENT type_constraint RPAREN COMMA TILDE LPAREN LIDENT type_constraint RPAREN %prec below_HASH
    {}

record_expr_content:
  separated_or_terminated_nonempty_list_SEMI_record_expr_field_
    {} [@name none_terminated_simple_expr_WITH_]
| simple_expr WITH separated_or_terminated_nonempty_list_SEMI_record_expr_field_
    {} [@name some_terminated_simple_expr_WITH_]

type_constraint:
  COLON core_type
    {}
| COLON core_type COLONGREATER core_type
    {}
| COLONGREATER core_type
    {}

newtypes:
  nonempty_list_newtype_
    {}

newtype:
  LIDENT
    {}
| LPAREN LIDENT COLON jkind_annotation RPAREN
    {}

pattern:
  pattern COLONCOLON pattern
    {}
| pattern attribute
    {}
| pattern_gen
    {}
| pattern AS val_ident
    {}
| pattern BAR pattern
    {}
| reversed_labeled_tuple_pattern_pattern_
    {}
| EXCEPTION ext list_attribute_ pattern %prec prec_constr_appl
    {}

pattern_no_exn:
  pattern_no_exn COLONCOLON pattern
    {}
| pattern_no_exn attribute
    {}
| pattern_gen
    {}
| pattern_no_exn AS val_ident
    {}
| pattern_no_exn BAR pattern
    {}
| reversed_labeled_tuple_pattern_pattern_no_exn_
    {}

labeled_tuple_pat_element_list_pattern_:
  labeled_tuple_pat_element_list_pattern_ COMMA pattern
    {}
| labeled_tuple_pat_element_list_pattern_ COMMA LABEL simple_pattern %prec COMMA
    {}
| labeled_tuple_pat_element_list_pattern_ COMMA TILDE LIDENT
    {}
| labeled_tuple_pat_element_list_pattern_ COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
    {}
| pattern COMMA pattern
    {}
| pattern COMMA LABEL simple_pattern %prec COMMA
    {}
| pattern COMMA TILDE LIDENT
    {}
| pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
    {}
| LABEL simple_pattern COMMA pattern
    {}
| LABEL simple_pattern COMMA LABEL simple_pattern %prec COMMA
    {}
| LABEL simple_pattern COMMA TILDE LIDENT
    {}
| LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
    {}
| TILDE LIDENT COMMA pattern
    {}
| TILDE LIDENT COMMA LABEL simple_pattern %prec COMMA
    {}
| TILDE LIDENT COMMA TILDE LIDENT
    {}
| TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
    {}
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA pattern
    {}
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA LABEL simple_pattern %prec COMMA
    {}
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LIDENT
    {}
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
    {}

labeled_tuple_pat_element_list_pattern_no_exn_:
  labeled_tuple_pat_element_list_pattern_no_exn_ COMMA pattern_no_exn
    {}
| labeled_tuple_pat_element_list_pattern_no_exn_ COMMA LABEL simple_pattern %prec COMMA
    {}
| labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LIDENT
    {}
| labeled_tuple_pat_element_list_pattern_no_exn_ COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
    {}
| pattern_no_exn COMMA pattern_no_exn
    {}
| pattern_no_exn COMMA LABEL simple_pattern %prec COMMA
    {}
| pattern_no_exn COMMA TILDE LIDENT
    {}
| pattern_no_exn COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
    {}
| LABEL simple_pattern COMMA pattern_no_exn
    {}
| LABEL simple_pattern COMMA LABEL simple_pattern %prec COMMA
    {}
| LABEL simple_pattern COMMA TILDE LIDENT
    {}
| LABEL simple_pattern COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
    {}
| TILDE LIDENT COMMA pattern_no_exn
    {}
| TILDE LIDENT COMMA LABEL simple_pattern %prec COMMA
    {}
| TILDE LIDENT COMMA TILDE LIDENT
    {}
| TILDE LIDENT COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
    {}
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA pattern_no_exn
    {}
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA LABEL simple_pattern %prec COMMA
    {}
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LIDENT
    {}
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA TILDE LPAREN LIDENT COLON core_type RPAREN %prec COMMA
    {}

reversed_labeled_tuple_pattern_pattern_:
  labeled_tuple_pat_element_list_pattern_ %prec below_COMMA
    {}
| labeled_tuple_pat_element_list_pattern_ COMMA DOTDOT
    {}
| pattern COMMA DOTDOT
    {}
| LABEL simple_pattern COMMA DOTDOT
    {}
| TILDE LIDENT COMMA DOTDOT
    {}
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
    {}

reversed_labeled_tuple_pattern_pattern_no_exn_:
  labeled_tuple_pat_element_list_pattern_no_exn_ %prec below_COMMA
    {}
| labeled_tuple_pat_element_list_pattern_no_exn_ COMMA DOTDOT
    {}
| pattern_no_exn COMMA DOTDOT
    {}
| LABEL simple_pattern COMMA DOTDOT
    {}
| TILDE LIDENT COMMA DOTDOT
    {}
| TILDE LPAREN LIDENT COLON core_type RPAREN COMMA DOTDOT
    {}

pattern_gen:
  simple_pattern
    {}
| constr_longident pattern %prec prec_constr_appl
    {}
| constr_longident LPAREN TYPE newtypes RPAREN simple_pattern
    {}
| constr_longident LPAREN TYPE LIDENT COLON jkind_annotation RPAREN simple_pattern
    {}
| name_tag pattern %prec prec_constr_appl
    {}
| LAZY ext list_attribute_ simple_pattern
    {}

simple_pattern:
  val_ident %prec below_EQUAL
    {}
| simple_pattern_not_ident
    {}

simple_pattern_not_ident:
  LPAREN pattern RPAREN
    {}
| simple_delimited_pattern
    {}
| LPAREN MODULE ext list_attribute_ module_name RPAREN
    {}
| LPAREN MODULE ext list_attribute_ module_name COLON module_type RPAREN
    {}
| UNDERSCORE
    {}
| signed_value_constant DOTDOT signed_value_constant
    {}
| constr_longident
    {}
| name_tag
    {}
| HASH type_longident
    {}
| HASH_SUFFIX type_longident
    {}
| mod_longident DOT simple_delimited_pattern
    {}
| mod_longident DOT LBRACKET RBRACKET
    {}
| mod_longident DOT LPAREN RPAREN
    {}
| mod_longident DOT LPAREN pattern RPAREN
    {}
| extension
    {}
| LPAREN pattern COLON core_type RPAREN
    {}
| signed_constant
    {}

simple_delimited_pattern:
  LBRACE listx_SEMI_record_pat_field_UNDERSCORE_ RBRACE
    {}
| HASHLBRACE listx_SEMI_record_pat_field_UNDERSCORE_ RBRACE
    {}
| LBRACKET separated_or_terminated_nonempty_list_SEMI_pattern_ RBRACKET
    {}
| LBRACKETBAR separated_or_terminated_nonempty_list_SEMI_pattern_ BARRBRACKET
    {}
| LBRACKETBAR BARRBRACKET
    {}
| LBRACKETCOLON separated_or_terminated_nonempty_list_SEMI_pattern_ COLONRBRACKET
    {}
| LBRACKETCOLON COLONRBRACKET
    {}
| HASHLPAREN reversed_labeled_tuple_pattern_pattern_ RPAREN
    {}

value_description:
  VAL ext list_attribute_ val_ident COLON possibly_poly_core_type_ optional_atat_modalities_expr list_post_item_attribute_
    {}

primitive_declaration:
  EXTERNAL ext list_attribute_ val_ident COLON possibly_poly_core_type_ optional_atat_modalities_expr EQUAL nonempty_list_raw_string_ list_post_item_attribute_
    {}

generic_type_declaration_no_nonrec_flag_type_subst_kind_:
  TYPE ext list_attribute_ type_parameters LIDENT option_jkind_constraint_ COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
    {}
| TYPE ext list_attribute_ NONREC type_parameters LIDENT option_jkind_constraint_ COLONEQUAL nonempty_type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
    {}

generic_type_declaration_nonrec_flag_type_kind_:
  TYPE ext list_attribute_ type_parameters LIDENT option_jkind_constraint_ type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
    {}
| TYPE ext list_attribute_ NONREC type_parameters LIDENT option_jkind_constraint_ type_kind reversed_llist_preceded_CONSTRAINT_constrain__ list_post_item_attribute_
    {}

nonempty_type_kind:
  core_type
    {}
| PRIVATE core_type
    {}
| constructor_declarations
    {} [@name none_terminated_core_type_EQUAL_]
| PRIVATE constructor_declarations
    {} [@name none_terminated_core_type_EQUAL_]
| core_type EQUAL constructor_declarations
    {} [@name some_terminated_core_type_EQUAL_]
| core_type EQUAL PRIVATE constructor_declarations
    {} [@name some_terminated_core_type_EQUAL_]
| DOTDOT
    {} [@name none_terminated_core_type_EQUAL_]
| PRIVATE DOTDOT
    {} [@name none_terminated_core_type_EQUAL_]
| core_type EQUAL DOTDOT
    {} [@name some_terminated_core_type_EQUAL_]
| core_type EQUAL PRIVATE DOTDOT
    {} [@name some_terminated_core_type_EQUAL_]
| LBRACE label_declarations RBRACE
    {} [@name none_terminated_core_type_EQUAL_]
| PRIVATE LBRACE label_declarations RBRACE
    {} [@name none_terminated_core_type_EQUAL_]
| core_type EQUAL LBRACE label_declarations RBRACE
    {} [@name some_terminated_core_type_EQUAL_]
| core_type EQUAL PRIVATE LBRACE label_declarations RBRACE
    {} [@name some_terminated_core_type_EQUAL_]
| HASHLBRACE label_declarations RBRACE
    {} [@name none_terminated_core_type_EQUAL_]
| PRIVATE HASHLBRACE label_declarations RBRACE
    {} [@name none_terminated_core_type_EQUAL_]
| core_type EQUAL HASHLBRACE label_declarations RBRACE
    {} [@name some_terminated_core_type_EQUAL_]
| core_type EQUAL PRIVATE HASHLBRACE label_declarations RBRACE
    {} [@name some_terminated_core_type_EQUAL_]

type_kind:
  
    {}
| EQUAL nonempty_type_kind
    {}

type_parameters:
  
    {}
| type_parameter
    {}
| LPAREN reversed_separated_nonempty_llist_COMMA_parenthesized_type_parameter_ RPAREN
    {}

jkind_desc:
  jkind_annotation MOD nonempty_list_mkrhs_LIDENT__
    {}
| jkind_annotation WITH core_type optional_atat_modalities_expr
    {}
| ident
    {}
| KIND_OF core_type
    {}
| UNDERSCORE
    {}
| reverse_product_jkind %prec below_AMPERSAND
    {}
| LPAREN jkind_desc RPAREN
    {}

reverse_product_jkind:
  jkind_annotation AMPERSAND jkind_annotation %prec prec_unboxed_product_kind
    {}
| reverse_product_jkind AMPERSAND jkind_annotation %prec prec_unboxed_product_kind
    {}

jkind_annotation:
  jkind_desc
    {}

jkind_constraint:
  COLON jkind_annotation
    {}

kind_abbreviation_decl:
  KIND_ABBREV LIDENT EQUAL jkind_annotation
    {}

parenthesized_type_parameter:
  type_parameter
    {}
| type_variance QUOTE ident list_attribute_ COLON jkind_annotation
    {}
| type_variance UNDERSCORE list_attribute_ COLON jkind_annotation
    {}

type_parameter:
  type_variance QUOTE ident list_attribute_
    {}
| type_variance UNDERSCORE list_attribute_
    {}

type_variance:
  
    {}
| PLUS
    {}
| MINUS
    {}
| BANG
    {}
| PLUS BANG
    {}
| BANG PLUS
    {}
| MINUS BANG
    {}
| BANG MINUS
    {}
| INFIXOP2
    {}
| PREFIXOP
    {}

constructor_declarations:
  BAR
    {}
| reversed_bar_llist_constructor_declaration_
    {}

generic_constructor_declaration_BAR_:
  BAR constr_ident generalized_constructor_arguments list_attribute_
    {}

generic_constructor_declaration_epsilon_:
  constr_ident generalized_constructor_arguments list_attribute_
    {}

str_exception_declaration:
  sig_exception_declaration
    {}
| EXCEPTION ext list_attribute_ constr_ident EQUAL constr_longident list_attribute_ list_post_item_attribute_
    {}

sig_exception_declaration:
  EXCEPTION ext list_attribute_ constr_ident generalized_constructor_arguments list_attribute_ list_post_item_attribute_
    {}

generalized_constructor_arguments:
  
    {}
| OF constructor_arguments
    {}
| COLON constructor_arguments MINUSGREATER atomic_type %prec below_HASH
    {}
| COLON reversed_nonempty_llist_typevar_ DOT constructor_arguments MINUSGREATER atomic_type %prec below_HASH
    {}
| COLON atomic_type %prec below_HASH
    {}
| COLON reversed_nonempty_llist_typevar_ DOT atomic_type %prec below_HASH
    {}

constructor_arguments:
  atomic_type optional_atat_modalities_expr
    {}
| GLOBAL atomic_type optional_atat_modalities_expr
    {}
| reversed_separated_nonempty_llist_STAR_constructor_argument_ STAR atomic_type optional_atat_modalities_expr
    {}
| reversed_separated_nonempty_llist_STAR_constructor_argument_ STAR GLOBAL atomic_type optional_atat_modalities_expr
    {}
| LBRACE label_declarations RBRACE
    {}

label_declarations:
  label_declaration
    {}
| label_declaration_semi
    {}
| label_declaration_semi label_declarations
    {}

label_declaration:
  mutable_or_global_flag LIDENT COLON possibly_poly_core_type_no_attr_ optional_atat_modalities_expr list_attribute_
    {}

label_declaration_semi:
  mutable_or_global_flag LIDENT COLON possibly_poly_core_type_no_attr_ optional_atat_modalities_expr list_attribute_ SEMI list_attribute_
    {}

extension_constructor_rebind_BAR_:
  BAR constr_ident EQUAL constr_longident list_attribute_
    {}

extension_constructor_rebind_epsilon_:
  constr_ident EQUAL constr_longident list_attribute_
    {}

with_constraint:
  TYPE type_parameters label_longident with_type_binder alias_type reversed_llist_preceded_CONSTRAINT_constrain__
    {}
| TYPE type_parameters label_longident COLONEQUAL alias_type
    {}
| MODULE mod_longident EQUAL mod_ext_longident
    {}
| MODULE mod_longident COLONEQUAL mod_ext_longident
    {}
| MODULE TYPE mty_longident EQUAL module_type
    {}
| MODULE TYPE mty_longident COLONEQUAL module_type
    {}

with_type_binder:
  EQUAL
    {}
| EQUAL PRIVATE
    {}

possibly_poly_core_type_:
  core_type
    {}
| reversed_nonempty_llist_typevar_ DOT core_type
    {}

possibly_poly_core_type_no_attr_:
  alias_type
    {}
| reversed_nonempty_llist_typevar_ DOT alias_type
    {}

core_type:
  alias_type
    {}
| core_type attribute
    {}

alias_type:
  function_type
    {}
| alias_type AS QUOTE ident
    {}
| alias_type AS LPAREN QUOTE ident COLON jkind_annotation RPAREN
    {}
| alias_type AS LPAREN UNDERSCORE COLON jkind_annotation RPAREN
    {}

function_type:
  tuple_type %prec MINUSGREATER
    {}
| strict_function_or_labeled_tuple_type
    {}

strict_function_or_labeled_tuple_type:
  optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| optlabel tuple_type MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| optlabel tuple_type at_mode_expr MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| optlabel nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| optlabel nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| optlabel nonempty_list_mode_legacy_ tuple_type MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| optlabel nonempty_list_mode_legacy_ tuple_type at_mode_expr MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| LIDENT COLON tuple_type MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| LIDENT COLON tuple_type at_mode_expr MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| LIDENT COLON nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| LIDENT COLON nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| LIDENT COLON nonempty_list_mode_legacy_ tuple_type MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| LIDENT COLON nonempty_list_mode_legacy_ tuple_type at_mode_expr MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| tuple_type MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| tuple_type at_mode_expr MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| nonempty_list_mode_legacy_ tuple_type MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| nonempty_list_mode_legacy_ tuple_type at_mode_expr MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| optlabel LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| optlabel tuple_type MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| optlabel tuple_type MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| optlabel tuple_type MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| optlabel tuple_type MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| optlabel tuple_type at_mode_expr MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| optlabel tuple_type at_mode_expr MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| optlabel tuple_type at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| optlabel tuple_type at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| optlabel nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| optlabel nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| optlabel nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| optlabel nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| optlabel nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| optlabel nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| optlabel nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| optlabel nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| optlabel nonempty_list_mode_legacy_ tuple_type MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| optlabel nonempty_list_mode_legacy_ tuple_type MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| optlabel nonempty_list_mode_legacy_ tuple_type MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| optlabel nonempty_list_mode_legacy_ tuple_type MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| optlabel nonempty_list_mode_legacy_ tuple_type at_mode_expr MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| optlabel nonempty_list_mode_legacy_ tuple_type at_mode_expr MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| optlabel nonempty_list_mode_legacy_ tuple_type at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| optlabel nonempty_list_mode_legacy_ tuple_type at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON tuple_type MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON tuple_type MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON tuple_type MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON tuple_type MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON tuple_type at_mode_expr MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON tuple_type at_mode_expr MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON tuple_type at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON tuple_type at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ tuple_type MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ tuple_type MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ tuple_type MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ tuple_type MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ tuple_type at_mode_expr MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ tuple_type at_mode_expr MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ tuple_type at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ tuple_type at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| tuple_type MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| tuple_type MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| tuple_type MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| tuple_type MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| tuple_type at_mode_expr MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| tuple_type at_mode_expr MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| tuple_type at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| tuple_type at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| nonempty_list_mode_legacy_ LPAREN reversed_nonempty_llist_typevar_ DOT core_type RPAREN at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| nonempty_list_mode_legacy_ tuple_type MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| nonempty_list_mode_legacy_ tuple_type MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| nonempty_list_mode_legacy_ tuple_type MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| nonempty_list_mode_legacy_ tuple_type MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| nonempty_list_mode_legacy_ tuple_type at_mode_expr MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| nonempty_list_mode_legacy_ tuple_type at_mode_expr MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| nonempty_list_mode_legacy_ tuple_type at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| nonempty_list_mode_legacy_ tuple_type at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ at_mode_expr MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| LIDENT COLON nonempty_list_mode_legacy_ atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| LIDENT COLON nonempty_list_mode_legacy_ atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ at_mode_expr MINUSGREATER strict_function_or_labeled_tuple_type
    {}
| LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ at_mode_expr MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ at_mode_expr MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ at_mode_expr MINUSGREATER tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ at_mode_expr MINUSGREATER tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type %prec MINUSGREATER
    {}
| LIDENT COLON nonempty_list_mode_legacy_ atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ at_mode_expr MINUSGREATER nonempty_list_mode_legacy_ tuple_type at_mode_expr %prec MINUSGREATER
    {}
| LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ %prec MINUSGREATER
    {}

at_mode_expr:
  AT nonempty_list_mode_
    {}

atat_modalities_expr:
  ATAT nonempty_list_modality_
    {}

optional_atat_modalities_expr:
   %prec below_HASH
    {}
| atat_modalities_expr
    {}

tuple_type:
  atomic_type %prec below_HASH
    {}
| atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ %prec below_FUNCTOR
    {}

delimited_type_supporting_local_open:
  LPAREN core_type RPAREN
    {}
| LPAREN MODULE ext list_attribute_ module_type RPAREN
    {}
| LBRACKET tag_field RBRACKET
    {}
| LBRACKET BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
    {}
| LBRACKET row_field BAR reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
    {}
| LBRACKETGREATER option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
    {}
| LBRACKETGREATER RBRACKET
    {}
| LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ RBRACKET
    {}
| LBRACKETLESS option_BAR_ reversed_separated_nonempty_llist_BAR_row_field_ GREATER reversed_nonempty_llist_name_tag_ RBRACKET
    {}
| HASHLPAREN atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ RPAREN
    {}
| HASHLPAREN LIDENT COLON atomic_type STAR reversed_separated_nonempty_llist_STAR_labeled_tuple_typ_element_ RPAREN
    {}

object_type:
  LESS meth_list GREATER
    {}
| LESS GREATER
    {}

extension_type:
  extension
    {}

delimited_type:
  object_type
    {}
| extension_type
    {}
| delimited_type_supporting_local_open
    {}

atomic_type:
  delimited_type
    {}
| type_longident
    {}
| atomic_type type_longident
    {}
| LPAREN reversed_separated_nontrivial_llist_COMMA_one_type_parameter_of_several_ RPAREN type_longident
    {}
| type_unboxed_longident
    {}
| atomic_type type_unboxed_longident
    {}
| LPAREN reversed_separated_nontrivial_llist_COMMA_one_type_parameter_of_several_ RPAREN type_unboxed_longident
    {}
| HASH clty_longident
    {}
| atomic_type HASH clty_longident
    {}
| LPAREN reversed_separated_nontrivial_llist_COMMA_one_type_parameter_of_several_ RPAREN HASH clty_longident
    {}
| mod_ext_longident DOT delimited_type_supporting_local_open
    {}
| QUOTE ident
    {}
| UNDERSCORE
    {}
| LPAREN QUOTE ident COLON jkind_annotation RPAREN
    {}
| LPAREN UNDERSCORE COLON jkind_annotation RPAREN
    {}

row_field:
  tag_field
    {}
| core_type
    {}

tag_field:
  name_tag OF opt_ampersand reversed_separated_nonempty_llist_AMPERSAND_core_type_no_attr_ list_attribute_
    {}
| name_tag list_attribute_
    {}

opt_ampersand:
  AMPERSAND
    {}
| 
    {}

meth_list:
  LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_ meth_list
    {}
| atomic_type SEMI meth_list
    {}
| LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_ SEMI list_attribute_
    {}
| atomic_type SEMI
    {}
| LIDENT COLON possibly_poly_core_type_no_attr_ list_attribute_
    {}
| atomic_type
    {}
| DOTDOT
    {}

value_constant:
  INT
    {}
| CHAR
    {}
| STRING
    {}
| FLOAT
    {}

unboxed_constant:
  HASH_INT
    {}
| HASH_FLOAT
    {}

constant:
  value_constant
    {}
| unboxed_constant
    {}

signed_value_constant:
  value_constant
    {}
| MINUS INT
    {}
| MINUS FLOAT
    {}
| PLUS INT
    {}
| PLUS FLOAT
    {}

signed_constant:
  signed_value_constant
    {}
| unboxed_constant
    {}
| MINUS HASH_INT
    {}
| MINUS HASH_FLOAT
    {}
| PLUS HASH_INT
    {}
| PLUS HASH_FLOAT
    {}

ident:
  UIDENT
    {}
| LIDENT
    {}

val_extra_ident:
  LPAREN operator RPAREN
    {}

val_ident:
  LIDENT
    {}
| val_extra_ident
    {}

operator:
  PREFIXOP
    {}
| LETOP
    {}
| ANDOP
    {}
| DOTOP LPAREN index_mod RPAREN
    {}
| DOTOP LPAREN index_mod RPAREN LESSMINUS
    {}
| DOTOP LBRACKET index_mod RBRACKET
    {}
| DOTOP LBRACKET index_mod RBRACKET LESSMINUS
    {}
| DOTOP LBRACE index_mod RBRACE
    {}
| DOTOP LBRACE index_mod RBRACE LESSMINUS
    {}
| HASHOP
    {}
| BANG
    {}
| INFIXOP0
    {}
| AT
    {}
| ATAT
    {}
| INFIXOP1
    {}
| INFIXOP2
    {}
| INFIXOP3
    {}
| MOD
    {}
| INFIXOP4
    {}
| PLUS
    {}
| PLUSDOT
    {}
| PLUSEQ
    {}
| MINUS
    {}
| MINUSDOT
    {}
| STAR
    {}
| PERCENT
    {}
| EQUAL
    {}
| LESS
    {}
| GREATER
    {}
| OR
    {}
| BARBAR
    {}
| AMPERSAND
    {}
| AMPERAMPER
    {}
| COLONEQUAL
    {}

index_mod:
  
    {}
| SEMI DOTDOT
    {}

constr_extra_nonprefix_ident:
  LBRACKET RBRACKET
    {}
| LPAREN RPAREN
    {}
| FALSE
    {}
| TRUE
    {}

constr_ident:
  UIDENT
    {}
| LPAREN COLONCOLON RPAREN
    {}
| constr_extra_nonprefix_ident
    {}

constr_longident:
  mod_longident %prec below_DOT
    {}
| mod_longident DOT LPAREN COLONCOLON RPAREN
    {}
| LPAREN COLONCOLON RPAREN
    {}
| constr_extra_nonprefix_ident
    {}

mk_longident_mod_ext_longident_LIDENT_:
  LIDENT
    {}
| mod_ext_longident DOT LIDENT
    {}

mk_longident_mod_ext_longident_UIDENT_:
  UIDENT
    {}
| mod_ext_longident DOT UIDENT
    {}

mk_longident_mod_ext_longident___anonymous_50_:
  ident
    {}
| LPAREN COLONCOLON RPAREN
    {}
| val_extra_ident
    {}
| mod_ext_longident DOT ident
    {}
| mod_ext_longident DOT LPAREN COLONCOLON RPAREN
    {}
| mod_ext_longident DOT val_extra_ident
    {}

mk_longident_mod_ext_longident_ident_:
  ident
    {}
| mod_ext_longident DOT ident
    {}

mk_longident_mod_ext_longident_type_trailing_hash_:
  type_trailing_hash
    {}
| mod_ext_longident DOT type_trailing_hash
    {}

mk_longident_mod_ext_longident_type_trailing_no_hash_:
  type_trailing_no_hash
    {}
| mod_ext_longident DOT type_trailing_no_hash
    {}

mk_longident_mod_longident_LIDENT_:
  LIDENT
    {}
| mod_longident DOT LIDENT
    {}

mk_longident_mod_longident_UIDENT_:
  UIDENT
    {}
| mod_longident DOT UIDENT
    {}

mk_longident_mod_longident_val_ident_:
  val_ident
    {}
| mod_longident DOT val_ident
    {}

val_longident:
  mk_longident_mod_longident_val_ident_
    {}

label_longident:
  mk_longident_mod_longident_LIDENT_
    {}

type_trailing_no_hash:
  LIDENT %prec below_HASH
    {}

type_trailing_hash:
  LIDENT HASH_SUFFIX
    {}

type_longident:
  mk_longident_mod_ext_longident_type_trailing_no_hash_
    {}

type_unboxed_longident:
  mk_longident_mod_ext_longident_type_trailing_hash_
    {}

mod_longident:
  mk_longident_mod_longident_UIDENT_
    {}

mod_ext_longident:
  mk_longident_mod_ext_longident_UIDENT_
    {}
| mod_ext_longident LPAREN mod_ext_longident RPAREN
    {}

mty_longident:
  mk_longident_mod_ext_longident_ident_
    {}

clty_longident:
  mk_longident_mod_ext_longident_LIDENT_
    {}

class_longident:
  mk_longident_mod_longident_LIDENT_
    {}

any_longident:
  mk_longident_mod_ext_longident___anonymous_50_
    {}
| constr_extra_nonprefix_ident
    {}

toplevel_directive:
  HASH ident
    {} [@name none_mk_directive_arg_toplevel_directive_argument_]
| HASH ident STRING
    {} [@name some_mk_directive_arg_toplevel_directive_argument_]
| HASH ident INT
    {} [@name some_mk_directive_arg_toplevel_directive_argument_]
| HASH ident val_longident
    {} [@name some_mk_directive_arg_toplevel_directive_argument_]
| HASH ident mod_longident
    {} [@name some_mk_directive_arg_toplevel_directive_argument_]
| HASH ident FALSE
    {} [@name some_mk_directive_arg_toplevel_directive_argument_]
| HASH ident TRUE
    {} [@name some_mk_directive_arg_toplevel_directive_argument_]
| HASH_SUFFIX ident
    {} [@name none_mk_directive_arg_toplevel_directive_argument_]
| HASH_SUFFIX ident STRING
    {} [@name some_mk_directive_arg_toplevel_directive_argument_]
| HASH_SUFFIX ident INT
    {} [@name some_mk_directive_arg_toplevel_directive_argument_]
| HASH_SUFFIX ident val_longident
    {} [@name some_mk_directive_arg_toplevel_directive_argument_]
| HASH_SUFFIX ident mod_longident
    {} [@name some_mk_directive_arg_toplevel_directive_argument_]
| HASH_SUFFIX ident FALSE
    {} [@name some_mk_directive_arg_toplevel_directive_argument_]
| HASH_SUFFIX ident TRUE
    {} [@name some_mk_directive_arg_toplevel_directive_argument_]

name_tag:
  BACKQUOTE ident
    {}

rec_flag:
  
    {}
| REC
    {}

direction_flag:
  TO
    {}
| DOWNTO
    {}

private_flag:
  
    {}
| PRIVATE
    {}

mutable_flag:
  
    {}
| MUTABLE
    {}

mutable_or_global_flag:
  
    {}
| MUTABLE
    {}
| GLOBAL
    {}

virtual_flag:
  
    {}
| VIRTUAL
    {}

mutable_virtual_flags:
  
    {}
| MUTABLE
    {}
| VIRTUAL
    {}
| MUTABLE VIRTUAL
    {}
| VIRTUAL MUTABLE
    {}

private_virtual_flags:
  
    {}
| PRIVATE
    {}
| VIRTUAL
    {}
| PRIVATE VIRTUAL
    {}
| VIRTUAL PRIVATE
    {}

virtual_with_mutable_flag:
  VIRTUAL
    {}
| MUTABLE VIRTUAL
    {}
| VIRTUAL MUTABLE
    {}

virtual_with_private_flag:
  VIRTUAL
    {}
| PRIVATE VIRTUAL
    {}
| VIRTUAL PRIVATE
    {}

subtractive:
  MINUS
    {}
| MINUSDOT
    {}

additive:
  PLUS
    {}
| PLUSDOT
    {}

optlabel:
  OPTLABEL
    {}
| QUESTION LIDENT COLON
    {}

single_attr_id:
  LIDENT
    {}
| UIDENT
    {}
| AND
    {}
| AS
    {}
| ASSERT
    {}
| BEGIN
    {}
| CLASS
    {}
| CONSTRAINT
    {}
| DO
    {}
| DONE
    {}
| DOWNTO
    {}
| ELSE
    {}
| END
    {}
| EXCEPTION
    {}
| EXTERNAL
    {}
| FALSE
    {}
| FOR
    {}
| FUN
    {}
| FUNCTION
    {}
| FUNCTOR
    {}
| IF
    {}
| IN
    {}
| INCLUDE
    {}
| INHERIT
    {}
| INITIALIZER
    {}
| LAZY
    {}
| LET
    {}
| LOCAL
    {}
| MATCH
    {}
| METHOD
    {}
| MODULE
    {}
| MUTABLE
    {}
| NEW
    {}
| NONREC
    {}
| OBJECT
    {}
| OF
    {}
| OPEN
    {}
| OR
    {}
| PRIVATE
    {}
| REC
    {}
| SIG
    {}
| STRUCT
    {}
| THEN
    {}
| TO
    {}
| TRUE
    {}
| TRY
    {}
| TYPE
    {}
| VAL
    {}
| VIRTUAL
    {}
| WHEN
    {}
| WHILE
    {}
| WITH
    {}

attr_id:
  single_attr_id
    {}
| single_attr_id DOT attr_id
    {}

attribute:
  LBRACKETAT attr_id attr_payload RBRACKET
    {}

post_item_attribute:
  LBRACKETATAT attr_id attr_payload RBRACKET
    {}

floating_attribute:
  LBRACKETATATAT attr_id attr_payload RBRACKET
    {}

ext:
  
    {}
| PERCENT attr_id
    {}

extension:
  LBRACKETPERCENT attr_id payload RBRACKET
    {}
| QUOTED_STRING_EXPR
    {}

item_extension:
  LBRACKETPERCENTPERCENT attr_id payload RBRACKET
    {}
| QUOTED_STRING_ITEM
    {}

payload:
  structure
    {}
| COLON signature
    {}
| COLON core_type
    {}
| QUESTION pattern
    {}
| QUESTION pattern WHEN seq_expr
    {}

attr_payload:
  payload
    {}

%%
