# some gold standard in the validation dataset were wrong, here we
# fix them.
library(tidyverse)

otiti_mixdb_path <- "../data/mixdb_otiti_tagged.rds"
mixdb_otiti_tagged <- readRDS(otiti_mixdb_path)
str(mixdb_otiti_tagged, 1L)


meta_db <- attr(mixdb_otiti_tagged, "meta")



to_one <- c(
    str_which(meta_db[["diario_1"]],
        "lavaggionasale broncovaleas __NUM__ puff x __NUM__ volte x __NUM__ gg becotide __NUM__ puff x __NUM__ volte x __NUM__ gg bentelan __NUM__ c x __NUM__ x __NUM__ gg < p > bentelan __NUM__cpr eff __NUM__ , __NUM__mg n ° conf . __NUM__ posologia : __NUM__ cpr , __NUM__ volte al gg . per __NUM__ gg . richiesta : visita oculistica controllo diagnosi di prescrizione : x visus richiesta : visita orl richiesta : impedenzometria certificato : visita orl richiesta : impedenzometria x rinite persistente"
    ),
    str_which(meta_db[["diario_1"]],
        "e . o . torace neg . otite mucosa bil gia ' in clenil adesso solo mucolitico"
    )
)

to_two <- c(
    str_which(meta_db[["diario_1"]],
        "< p > augmentin bb sosp fl__NUM__ml c / sir n ° conf . __NUM__ otite sierosa dx"
    ),
    str_which(meta_db[["diario_1"]],
        "tosse e vomito febbre alternante __NUM__ naso chiuso e otalgia mt sx un po ' arrossata naso chiuso stiamo a vedere"
    ),
    str_which(meta_db[["diario_1"]],
        "richieste : esame colturale tampone auricolare dx o sx quesito : persistenza di otorrea orecchio dx dopo __NUM__ cicli anticiotici"
    ),
    str_which(meta_db[["diario_1"]],
        "rinite da __NUM__ giorni otorrea a dx da stanotte tosse sporadica"
    ),
    str_which(meta_db[["diario_1"]],
        "richieste : esame colturale tampone auricolare dx o sx quesito : persistenza di otorrea orecchio dx dopo __NUM__ cicli anticiotici"
    ),
    str_which(meta_db[["diario_1"]],
        "otitinite . eruzione premolare dxe media dx febbre r __NUM__ : __NUM__ - __NUM__ / __NUM__ / __NUM__ - peso : __NUM__ , alt : __NUM__ , bmi : __NUM__ < p > augmentin bb sosp fl__NUM__ml c / sir n ° conf . __NUM__"
    ),
    intersect(
        str_which(meta_db[["diagnosi1"]],
            "^otalgia$"
        ),
        str_which(meta_db[["diario_1"]],
            "^otite sierosa$"
        )
    )
)

to_three <- c(
    str_which(meta_db[["diagnosi1"]],
        "dolore orecchio dx alla masticazione e deglutizione in terapia con augmentin sugg x oma"
    ),
    str_which(meta_db[["diario_1"]],
        "nn mmigliorato l ' orecchio e si sopetta prenda male l ' antibiotico , maqndato a vis orl urgente , visto dr d ' agnone e dato rocefin fiale i . m . da __NUM__ x __NUM__ gg"
    ),
    str_which(meta_db[["diario_1"]],
        "otorrea a dx , mt opaca , non perfettamente visibile < p > cefixoral __NUM__cpr disp __NUM__mg n ° conf . __NUM__ per otite media acuta < __NUM__ >"
    )
)

to_four <- c(
    str_which(meta_db[["diario_1"]],
        "da ieri febbre , otorrea sx < p > neoduplamox bb sosp fl__NUM__ml c / c n"
    ),
    str_which(meta_db[["diario_1"]],
        "< p > glazidim im __NUM__fl __NUM__mg / __NUM__ , __NUM__ml "
    ),
    str_which(meta_db[["diario_1"]],
        "da ieri febbre , otorrea sx < p > neoduplamox bb sosp "
    )

)

mixdb_otiti_tagged_old <- mixdb_otiti_tagged

mixdb_otiti_tagged$y[to_one]   <- 1L
mixdb_otiti_tagged$y[to_two]   <- 2L
mixdb_otiti_tagged$y[to_three] <- 3L
mixdb_otiti_tagged$y[to_four]  <- 4L


saveRDS(mixdb_otiti_tagged, otiti_mixdb_path)
saveRDS(mixdb_otiti_tagged_old, paste0(otiti_mixdb_path, ".old3"))
