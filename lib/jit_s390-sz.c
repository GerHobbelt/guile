
#if __WORDSIZE == 32
#define JIT_INSTR_MAX 50
    0,  /* data */
    0,  /* live */
    2,  /* align */
    0,  /* save */
    0,  /* load */
    0,  /* #name */
    0,  /* #note */
    2,  /* label */
    38, /* prolog */
    0,  /* arg */
    4,  /* addr */
    12, /* addi */
    4,  /* addcr */
    10, /* addci */
    6,  /* addxr */
    10, /* addxi */
    6,  /* subr */
    12, /* subi */
    6,  /* subcr */
    10, /* subci */
    8,  /* subxr */
    10, /* subxi */
    14, /* rsbi */
    6,  /* mulr */
    14, /* muli */
    46, /* qmulr */
    50, /* qmuli */
    10, /* qmulr_u */
    18, /* qmuli_u */
    10, /* divr */
    18, /* divi */
    16, /* divr_u */
    24, /* divi_u */
    12, /* qdivr */
    16, /* qdivi */
    18, /* qdivr_u */
    22, /* qdivi_u */
    10, /* remr */
    18, /* remi */
    16, /* remr_u */
    24, /* remi_u */
    4,  /* andr */
    10, /* andi */
    4,  /* orr */
    10, /* ori */
    4,  /* xorr */
    12, /* xori */
    8,  /* lshr */
    10, /* lshi */
    8,  /* rshr */
    10, /* rshi */
    8,  /* rshr_u */
    10, /* rshi_u */
    2,  /* negr */
    8,  /* comr */
    16, /* ltr */
    20, /* lti */
    16, /* ltr_u */
    20, /* lti_u */
    16, /* ler */
    20, /* lei */
    16, /* ler_u */
    20, /* lei_u */
    16, /* eqr */
    20, /* eqi */
    16, /* ger */
    20, /* gei */
    16, /* ger_u */
    20, /* gei_u */
    16, /* gtr */
    20, /* gti */
    16, /* gtr_u */
    20, /* gti_u */
    16, /* ner */
    20, /* nei */
    2,  /* movr */
    8,  /* movi */
    4,  /* extr_c */
    4,  /* extr_uc */
    4,  /* extr_s */
    4,  /* extr_us */
    0,  /* extr_i */
    0,  /* extr_ui */
    4,  /* htonr_us */
    2,  /* htonr_ui */
    0,  /* htonr_ul */
    6,  /* ldr_c */
    12, /* ldi_c */
    6,  /* ldr_uc */
    14, /* ldi_uc */
    6,  /* ldr_s */
    12, /* ldi_s */
    6,  /* ldr_us */
    12, /* ldi_us */
    6,  /* ldr_i */
    12, /* ldi_i */
    0,  /* ldr_ui */
    0,  /* ldi_ui */
    0,  /* ldr_l */
    0,  /* ldi_l */
    10, /* ldxr_c */
    16, /* ldxi_c */
    10, /* ldxr_uc */
    16, /* ldxi_uc */
    10, /* ldxr_s */
    16, /* ldxi_s */
    10, /* ldxr_us */
    16, /* ldxi_us */
    10, /* ldxr_i */
    16, /* ldxi_i */
    0,  /* ldxr_ui */
    0,  /* ldxi_ui */
    0,  /* ldxr_l */
    0,  /* ldxi_l */
    4,  /* str_c */
    12, /* sti_c */
    4,  /* str_s */
    10, /* sti_s */
    4,  /* str_i */
    10, /* sti_i */
    0,  /* str_l */
    0,  /* sti_l */
    8,  /* stxr_c */
    16, /* stxi_c */
    8,  /* stxr_s */
    16, /* stxi_s */
    8,  /* stxr_i */
    16, /* stxi_i */
    0,  /* stxr_l */
    0,  /* stxi_l */
    8,  /* bltr */
    12, /* blti */
    8,  /* bltr_u */
    12, /* blti_u */
    8,  /* bler */
    12, /* blei */
    8,  /* bler_u */
    12, /* blei_u */
    8,  /* beqr */
    16, /* beqi */
    8,  /* bger */
    12, /* bgei */
    8,  /* bger_u */
    12, /* bgei_u */
    8,  /* bgtr */
    12, /* bgti */
    8,  /* bgtr_u */
    12, /* bgti_u */
    8,  /* bner */
    16, /* bnei */
    12, /* bmsr */
    14, /* bmsi */
    12, /* bmcr */
    14, /* bmci */
    8,  /* boaddr */
    12, /* boaddi */
    8,  /* boaddr_u */
    12, /* boaddi_u */
    8,  /* bxaddr */
    12, /* bxaddi */
    8,  /* bxaddr_u */
    12, /* bxaddi_u */
    8,  /* bosubr */
    12, /* bosubi */
    8,  /* bosubr_u */
    12, /* bosubi_u */
    8,  /* bxsubr */
    12, /* bxsubi */
    8,  /* bxsubr_u */
    12, /* bxsubi_u */
    2,  /* jmpr */
    10, /* jmpi */
    2,  /* callr */
    10, /* calli */
    36, /* epilog */
    0,  /* arg_f */
    6,  /* addr_f */
    24, /* addi_f */
    8,  /* subr_f */
    24, /* subi_f */
    28, /* rsbi_f */
    6,  /* mulr_f */
    24, /* muli_f */
    8,  /* divr_f */
    24, /* divi_f */
    4,  /* negr_f */
    4,  /* absr_f */
    4,  /* sqrtr_f */
    16, /* ltr_f */
    36, /* lti_f */
    16, /* ler_f */
    36, /* lei_f */
    16, /* eqr_f */
    36, /* eqi_f */
    16, /* ger_f */
    36, /* gei_f */
    16, /* gtr_f */
    36, /* gti_f */
    16, /* ner_f */
    36, /* nei_f */
    16, /* unltr_f */
    36, /* unlti_f */
    16, /* unler_f */
    36, /* unlei_f */
    20, /* uneqr_f */
    40, /* uneqi_f */
    16, /* unger_f */
    36, /* ungei_f */
    16, /* ungtr_f */
    36, /* ungti_f */
    20, /* ltgtr_f */
    40, /* ltgti_f */
    16, /* ordr_f */
    36, /* ordi_f */
    16, /* unordr_f */
    36, /* unordi_f */
    4,  /* truncr_f_i */
    0,  /* truncr_f_l */
    4,  /* extr_f */
    4,  /* extr_d_f */
    2,  /* movr_f */
    20, /* movi_f */
    4,  /* ldr_f */
    10, /* ldi_f */
    8,  /* ldxr_f */
    14, /* ldxi_f */
    4,  /* str_f */
    10, /* sti_f */
    8,  /* stxr_f */
    14, /* stxi_f */
    10, /* bltr_f */
    28, /* blti_f */
    10, /* bler_f */
    30, /* blei_f */
    10, /* beqr_f */
    30, /* beqi_f */
    10, /* bger_f */
    30, /* bgei_f */
    10, /* bgtr_f */
    30, /* bgti_f */
    10, /* bner_f */
    30, /* bnei_f */
    10, /* bunltr_f */
    28, /* bunlti_f */
    10, /* bunler_f */
    28, /* bunlei_f */
    18, /* buneqr_f */
    36, /* buneqi_f */
    10, /* bunger_f */
    30, /* bungei_f */
    10, /* bungtr_f */
    30, /* bungti_f */
    18, /* bltgtr_f */
    38, /* bltgti_f */
    10, /* bordr_f */
    30, /* bordi_f */
    10, /* bunordr_f */
    28, /* bunordi_f */
    0,  /* arg_d */
    6,  /* addr_d */
    34, /* addi_d */
    8,  /* subr_d */
    34, /* subi_d */
    38, /* rsbi_d */
    6,  /* mulr_d */
    34, /* muli_d */
    8,  /* divr_d */
    34, /* divi_d */
    4,  /* negr_d */
    4,  /* absr_d */
    4,  /* sqrtr_d */
    16, /* ltr_d */
    46, /* lti_d */
    16, /* ler_d */
    46, /* lei_d */
    16, /* eqr_d */
    46, /* eqi_d */
    16, /* ger_d */
    46, /* gei_d */
    16, /* gtr_d */
    46, /* gti_d */
    16, /* ner_d */
    46, /* nei_d */
    16, /* unltr_d */
    46, /* unlti_d */
    16, /* unler_d */
    46, /* unlei_d */
    20, /* uneqr_d */
    50, /* uneqi_d */
    16, /* unger_d */
    46, /* ungei_d */
    16, /* ungtr_d */
    46, /* ungti_d */
    20, /* ltgtr_d */
    50, /* ltgti_d */
    16, /* ordr_d */
    46, /* ordi_d */
    16, /* unordr_d */
    46, /* unordi_d */
    4,  /* truncr_d_i */
    0,  /* truncr_d_l */
    4,  /* extr_d */
    4,  /* extr_f_d */
    2,  /* movr_d */
    30, /* movi_d */
    4,  /* ldr_d */
    10, /* ldi_d */
    8,  /* ldxr_d */
    14, /* ldxi_d */
    4,  /* str_d */
    10, /* sti_d */
    8,  /* stxr_d */
    14, /* stxi_d */
    10, /* bltr_d */
    38, /* blti_d */
    10, /* bler_d */
    38, /* blei_d */
    10, /* beqr_d */
    40, /* beqi_d */
    10, /* bger_d */
    40, /* bgei_d */
    10, /* bgtr_d */
    40, /* bgti_d */
    10, /* bner_d */
    40, /* bnei_d */
    10, /* bunltr_d */
    38, /* bunlti_d */
    10, /* bunler_d */
    38, /* bunlei_d */
    18, /* buneqr_d */
    46, /* buneqi_d */
    10, /* bunger_d */
    40, /* bungei_d */
    10, /* bungtr_d */
    40, /* bungti_d */
    18, /* bltgtr_d */
    48, /* bltgti_d */
    10, /* bordr_d */
    40, /* bordi_d */
    10, /* bunordr_d */
    38, /* bunordi_d */
    0,  /* movr_w_f */
    0,  /* movr_ww_d */
    0,  /* movr_w_d */
    0,  /* movr_f_w */
    0,  /* movi_f_w */
    0,  /* movr_d_ww */
    0,  /* movi_d_ww */
    0,  /* movr_d_w */
    0,  /* movi_d_w */
    0,  /* x86_retval_f */
    0,  /* x86_retval_d */
    0,	/* va_start */
    0,	/* va_arg */
    0,	/* va_arg_d */
    0,	/* va_end */
#endif /* __WORDSIZE */

#if __WORDSIZE == 64
#define JIT_INSTR_MAX 68
    0,	/* data */
    0,	/* live */
    6,	/* align */
    0,	/* save */
    0,	/* load */
    0,	/* #name */
    0,	/* #note */
    2,	/* label */
    38,	/* prolog */
    0,	/* arg */
    8,	/* addr */
    24,	/* addi */
    8,	/* addcr */
    20,	/* addci */
    8,	/* addxr */
    12,	/* addxi */
    12,	/* subr */
    24,	/* subi */
    12,	/* subcr */
    20,	/* subci */
    12,	/* subxr */
    12,	/* subxi */
    28,	/* rsbi */
    8,	/* mulr */
    24,	/* muli */
    60,	/* qmulr */
    68,	/* qmuli */
    16,	/* qmulr_u */
    32,	/* qmuli_u */
    12,	/* divr */
    28,	/* divi */
    16,	/* divr_u */
    32,	/* divi_u */
    16,	/* qdivr */
    20,	/* qdivi */
    20,	/* qdivr_u */
    24,	/* qdivi_u */
    12,	/* remr */
    28,	/* remi */
    16,	/* remr_u */
    32,	/* remi_u */
    8,	/* andr */
    20,	/* andi */
    8,	/* orr */
    20,	/* ori */
    8,	/* xorr */
    24,	/* xori */
    6,	/* lshr */
    10,	/* lshi */
    6,	/* rshr */
    10,	/* rshi */
    6,	/* rshr_u */
    10,	/* rshi_u */
    4,	/* negr */
    12,	/* comr */
    20,	/* ltr */
    24,	/* lti */
    20,	/* ltr_u */
    24,	/* lti_u */
    20,	/* ler */
    24,	/* lei */
    20,	/* ler_u */
    24,	/* lei_u */
    20,	/* eqr */
    24,	/* eqi */
    20,	/* ger */
    24,	/* gei */
    20,	/* ger_u */
    24,	/* gei_u */
    20,	/* gtr */
    24,	/* gti */
    20,	/* gtr_u */
    24,	/* gti_u */
    20,	/* ner */
    24,	/* nei */
    4,	/* movr */
    16,	/* movi */
    4,	/* extr_c */
    4,	/* extr_uc */
    4,	/* extr_s */
    4,	/* extr_us */
    4,	/* extr_i */
    4,	/* extr_ui */
    4,	/* htonr_us */
    4,	/* htonr_ui */
    4,	/* htonr_ul */
    6,	/* ldr_c */
    18,	/* ldi_c */
    6,	/* ldr_uc */
    18,	/* ldi_uc */
    6,	/* ldr_s */
    18,	/* ldi_s */
    6,	/* ldr_us */
    18,	/* ldi_us */
    6,	/* ldr_i */
    18,	/* ldi_i */
    6,	/* ldr_ui */
    18,	/* ldi_ui */
    6,	/* ldr_l */
    18,	/* ldi_l */
    14,	/* ldxr_c */
    6,	/* ldxi_c */
    14,	/* ldxr_uc */
    6,	/* ldxi_uc */
    14,	/* ldxr_s */
    6,	/* ldxi_s */
    14,	/* ldxr_us */
    6,	/* ldxi_us */
    14,	/* ldxr_i */
    6,	/* ldxi_i */
    14,	/* ldxr_ui */
    6,	/* ldxi_ui */
    14,	/* ldxr_l */
    6,	/* ldxi_l */
    4,	/* str_c */
    16,	/* sti_c */
    4,	/* str_s */
    16,	/* sti_s */
    4,	/* str_i */
    16,	/* sti_i */
    6,	/* str_l */
    18,	/* sti_l */
    12,	/* stxr_c */
    4,	/* stxi_c */
    12,	/* stxr_s */
    4,	/* stxi_s */
    12,	/* stxr_i */
    6,	/* stxi_i */
    14,	/* stxr_l */
    6,	/* stxi_l */
    10,	/* bltr */
    14,	/* blti */
    10,	/* bltr_u */
    14,	/* blti_u */
    10,	/* bler */
    14,	/* blei */
    10,	/* bler_u */
    14,	/* blei_u */
    10,	/* beqr */
    26,	/* beqi */
    10,	/* bger */
    14,	/* bgei */
    10,	/* bger_u */
    14,	/* bgei_u */
    10,	/* bgtr */
    14,	/* bgti */
    10,	/* bgtr_u */
    14,	/* bgti_u */
    10,	/* bner */
    26,	/* bnei */
    18,	/* bmsr */
    18,	/* bmsi */
    18,	/* bmcr */
    18,	/* bmci */
    10,	/* boaddr */
    14,	/* boaddi */
    10,	/* boaddr_u */
    14,	/* boaddi_u */
    10,	/* bxaddr */
    14,	/* bxaddi */
    10,	/* bxaddr_u */
    14,	/* bxaddi_u */
    10,	/* bosubr */
    14,	/* bosubi */
    10,	/* bosubr_u */
    14,	/* bosubi_u */
    10,	/* bxsubr */
    14,	/* bxsubi */
    10,	/* bxsubr_u */
    14,	/* bxsubi_u */
    0,	/* jmpr */
    18,	/* jmpi */
    2,	/* callr */
    18,	/* calli */
    36,	/* epilog */
    0,	/* arg_f */
    6,	/* addr_f */
    26,	/* addi_f */
    8,	/* subr_f */
    26,	/* subi_f */
    26,	/* rsbi_f */
    6,	/* mulr_f */
    26,	/* muli_f */
    8,	/* divr_f */
    26,	/* divi_f */
    4,	/* negr_f */
    4,	/* absr_f */
    4,	/* sqrtr_f */
    16,	/* ltr_f */
    36,	/* lti_f */
    16,	/* ler_f */
    36,	/* lei_f */
    16,	/* eqr_f */
    36,	/* eqi_f */
    16,	/* ger_f */
    36,	/* gei_f */
    16,	/* gtr_f */
    36,	/* gti_f */
    16,	/* ner_f */
    36,	/* nei_f */
    16,	/* unltr_f */
    36,	/* unlti_f */
    16,	/* unler_f */
    36,	/* unlei_f */
    20,	/* uneqr_f */
    40,	/* uneqi_f */
    16,	/* unger_f */
    36,	/* ungei_f */
    16,	/* ungtr_f */
    36,	/* ungti_f */
    20,	/* ltgtr_f */
    40,	/* ltgti_f */
    16,	/* ordr_f */
    36,	/* ordi_f */
    16,	/* unordr_f */
    36,	/* unordi_f */
    4,	/* truncr_f_i */
    4,	/* truncr_f_l */
    4,	/* extr_f */
    4,	/* extr_d_f */
    2,	/* movr_f */
    20,	/* movi_f */
    4,	/* ldr_f */
    16,	/* ldi_f */
    12,	/* ldxr_f */
    4,	/* ldxi_f */
    4,	/* str_f */
    16,	/* sti_f */
    12,	/* stxr_f */
    4,	/* stxi_f */
    10,	/* bltr_f */
    30,	/* blti_f */
    10,	/* bler_f */
    30,	/* blei_f */
    10,	/* beqr_f */
    30,	/* beqi_f */
    10,	/* bger_f */
    30,	/* bgei_f */
    10,	/* bgtr_f */
    30,	/* bgti_f */
    10,	/* bner_f */
    30,	/* bnei_f */
    10,	/* bunltr_f */
    30,	/* bunlti_f */
    10,	/* bunler_f */
    30,	/* bunlei_f */
    18,	/* buneqr_f */
    38,	/* buneqi_f */
    10,	/* bunger_f */
    30,	/* bungei_f */
    10,	/* bungtr_f */
    30,	/* bungti_f */
    18,	/* bltgtr_f */
    38,	/* bltgti_f */
    10,	/* bordr_f */
    30,	/* bordi_f */
    10,	/* bunordr_f */
    30,	/* bunordi_f */
    0,	/* arg_d */
    6,	/* addr_d */
    26,	/* addi_d */
    8,	/* subr_d */
    26,	/* subi_d */
    26,	/* rsbi_d */
    6,	/* mulr_d */
    26,	/* muli_d */
    8,	/* divr_d */
    26,	/* divi_d */
    4,	/* negr_d */
    4,	/* absr_d */
    4,	/* sqrtr_d */
    16,	/* ltr_d */
    36,	/* lti_d */
    16,	/* ler_d */
    36,	/* lei_d */
    16,	/* eqr_d */
    36,	/* eqi_d */
    16,	/* ger_d */
    36,	/* gei_d */
    16,	/* gtr_d */
    36,	/* gti_d */
    16,	/* ner_d */
    36,	/* nei_d */
    16,	/* unltr_d */
    36,	/* unlti_d */
    16,	/* unler_d */
    36,	/* unlei_d */
    20,	/* uneqr_d */
    40,	/* uneqi_d */
    16,	/* unger_d */
    36,	/* ungei_d */
    16,	/* ungtr_d */
    36,	/* ungti_d */
    20,	/* ltgtr_d */
    40,	/* ltgti_d */
    16,	/* ordr_d */
    36,	/* ordi_d */
    16,	/* unordr_d */
    36,	/* unordi_d */
    4,	/* truncr_d_i */
    4,	/* truncr_d_l */
    4,	/* extr_d */
    4,	/* extr_f_d */
    2,	/* movr_d */
    24,	/* movi_d */
    4,	/* ldr_d */
    16,	/* ldi_d */
    12,	/* ldxr_d */
    4,	/* ldxi_d */
    4,	/* str_d */
    16,	/* sti_d */
    12,	/* stxr_d */
    4,	/* stxi_d */
    10,	/* bltr_d */
    30,	/* blti_d */
    10,	/* bler_d */
    30,	/* blei_d */
    10,	/* beqr_d */
    34,	/* beqi_d */
    10,	/* bger_d */
    30,	/* bgei_d */
    10,	/* bgtr_d */
    30,	/* bgti_d */
    10,	/* bner_d */
    30,	/* bnei_d */
    10,	/* bunltr_d */
    30,	/* bunlti_d */
    10,	/* bunler_d */
    30,	/* bunlei_d */
    18,	/* buneqr_d */
    38,	/* buneqi_d */
    10,	/* bunger_d */
    30,	/* bungei_d */
    10,	/* bungtr_d */
    30,	/* bungti_d */
    18,	/* bltgtr_d */
    38,	/* bltgti_d */
    10,	/* bordr_d */
    30,	/* bordi_d */
    10,	/* bunordr_d */
    30,	/* bunordi_d */
    0,	/* movr_w_f */
    0,	/* movr_ww_d */
    0,	/* movr_w_d */
    0,	/* movr_f_w */
    0,	/* movi_f_w */
    0,	/* movr_d_ww */
    0,	/* movi_d_ww */
    0,	/* movr_d_w */
    0,	/* movi_d_w */
    0,	/* x86_retval_f */
    0,	/* x86_retval_d */
    0,	/* va_start */
    0,	/* va_arg */
    0,	/* va_arg_d */
    0,	/* va_end */
#endif /* __WORDSIZE */
