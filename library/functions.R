#install packages, if they are not installed

package_check <- function(x) {
    
    lapply(x,
           
           FUN <- function(y) {
               
               if (!require(y, character.only = TRUE)) {
                   
                   install.packages(y, dependencies = TRUE)
                   
                   library(y, character.only = TRUE, warn.conflicts = FALSE)
                   
               } else {
                   
                   library(y, character.only = TRUE, warn.conflicts = FALSE)
                   
               }
           }
    )
    
}

#Calulate juros_diario

juros <- function(x) {
    
    temp <- NULL
    
    for (l in 1 : length(x)) {
        
        z <- x[[l]] %>% select(id, divida_total, pagamento_diario) 
        
        for (r in 1 : dim(z)[1]) {
            
            if (dim(z)[1] == 1) {
                
                juros_diario = 0
                juros_temp = as.vector(unlist((c("id" = z[1,1], juros_diario))))
                temp <- rbind(temp, juros_temp)
                break
            }
            
            else if (dim(z)[1] == 2) {
                
                juros_diario = 1- ((z[r,2] - z[r+1,3]) / z[r+1,2])
                juros_temp = as.vector(unlist((c("id" = z[1,1], juros_diario))))
                temp <- rbind(temp, juros_temp)
                break
            }
            
            else if (z[r,2] < z[r+1,2]) {
                
                juros_diario = (z[r+1,2] / z[r,2]) - 1
                juros_temp = as.vector(unlist((c("id" = z[1,1], juros_diario))))
                temp <- rbind(temp, juros_temp)
                break 
            }
            
            else if (r == dim(z)[1] - 1) {
                
                juros_diario = ((z[r,2] + z[r-1,3]) / z[r-1,2]) - 1
                juros_temp = as.vector(unlist((c("id" = z[1,1], juros_diario))))
                temp <- rbind(temp, juros_temp)
                break
            }
                
        }

    }
    colnames(temp) <- c("id", "juros") 
    temp <- as_tibble(temp)
    #temp <- temp %>%mutate(juros = ifelse(juros < 0,0))%>%filter(!(juros > 0.01))
    return(temp)
}

# Get Mode
mode <- function(x) {
    uniqv <- unique(x)
    uniqv[which.max(tabulate(match(x, uniqv)))]
}

#Calculate taxa_transacao

taxa <- function(x) {
    
    db_vector_tx <- NULL
    
    for (l in 1 : length(x)) {
                
            tx <- x[[l]]%>% pull(taxa_transacao)
            tx<- mode(tx)
            vector_tx <- c((x[[l]]%>% pull(id))[1],tx)
            db_vector_tx <- rbind(db_vector_tx, vector_tx)
            
    }
    colnames(db_vector_tx) <- c("id", "taxa_transacao")
    db_vector_tx <- as_tibble(db_vector_tx)
    return(db_vector_tx)
}

#Calculate diasdepois_x

diasdepois <- function(x, type = "a" | "p" | "t") {
    
    db_diasdepois <- NULL 
    
    if( type == "a") {
        
            z <- x %>%
            select(id, dias_pos_desembolso, amortizacao_principal_diario) %>%
            filter(amortizacao_principal_diario != 0) %>%
            slice_head(n = 1) %>%
            mutate(diasdepois_amort = dias_pos_desembolso)
            
            z <- juros_d %>% left_join(z, by = c("id" = "id"))%>%
            mutate(diasdepois_amort = replace_na(diasdepois_amort, "Missed 90 days"))%>%
            select(-dias_pos_desembolso, -amortizacao_principal_diario, - juros_d_nivel)%>%
            group_by(id) %>%
            group_split()
        
    } else if( type == "p") {
        
        z <- x %>%
            select(id, dias_pos_desembolso, pagamento_diario) %>%
            filter(pagamento_diario != 0) %>%
            slice_head(n = 1) %>%
            mutate(diasdepois_pgto = dias_pos_desembolso)
        
        z <- juros_d %>% left_join(z, by = c("id" = "id"))%>%
            mutate(diasdepois_pgto = replace_na(diasdepois_pgto, "Missed 90 days"))%>%
            select(-dias_pos_desembolso, -pagamento_diario, - juros_d_nivel)%>%
            group_by(id) %>%
            group_split()
        
    } else if( type == "t") {
        
        z <- x %>%
            select(id, dias_pos_desembolso, transacionado) %>%
            filter(transacionado != 0) %>%
            slice_head(n = 1) %>%
            mutate(diasdepois_trans = dias_pos_desembolso)
        
        z <- juros_d %>% left_join(z, by = c("id" = "id"))%>%
            mutate(diasdepois_trans = replace_na(diasdepois_trans, "Missed 90 days"))%>%
            select(-dias_pos_desembolso, -transacionado, - juros_d_nivel)%>%
            group_by(id) %>%
            group_split()
        
    }
    
    for (l in 1 : length(z)) {
        
        dias_x <- c((z[[l]]%>% pull(id))[1],(z[[l]]%>% pull(colnames(z[[l]])[2]))[1])
        db_diasdepois <- rbind(db_diasdepois, dias_x)
        
    }
    colnames(db_diasdepois) <- c("id", colnames(z[[l]])[2])
    db_diasdepois <- as_tibble(db_diasdepois)
    return(db_diasdepois)
}


#Calculate liquidacao_x

liquidacao <- function(x) {
    
    db_x_liquidation <- NULL
    
    z <- x %>%
        select(id, dias_pos_desembolso, valor_emprestado, divida_principal) %>%
        group_by(id) %>%
        group_split()
    
    nov_25 = round(0.25 * 90, 0)
    nov_50 = round(0.50 * 90, 0)
    nov_75 = round(0.75 * 90, 0)
    nov_100 = 90
    
    for (l in 1 : length(z)) {
        
        if (dim(z[[l]])[1] > 90) {
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso <= nov_25)
            liq_25 <- 1 - (secondary_tbl%>%pull(divida_principal))[dim(secondary_tbl)[1]] / (secondary_tbl%>%pull(valor_emprestado))[1]
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso > nov_25 & dias_pos_desembolso <= nov_50)
            liq_50 <- 1 - (secondary_tbl%>%pull(divida_principal))[dim(secondary_tbl)[1]] / (secondary_tbl%>%pull(valor_emprestado))[1]
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso > nov_50 & dias_pos_desembolso <= nov_75)
            liq_75 <- 1 - (secondary_tbl%>%pull(divida_principal))[dim(secondary_tbl)[1]] / (secondary_tbl%>%pull(valor_emprestado))[1]
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso > nov_75)
            liq_dia_90 <- 1- (secondary_tbl%>%pull(divida_principal))[1] / (secondary_tbl%>%pull(valor_emprestado))[1]
            
            point_x <- c((z[[l]]%>% pull(id))[1],liq_25, liq_50, liq_75, liq_dia_90)
            db_x_liquidation <- rbind(db_x_liquidation, point_x)
            
        } else if (dim(z[[l]])[1] >= 4 & dim(z[[l]])[1] <= 90) {
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso <= dim(z[[l]])[1]*0.25)
            liq_25 <- 1 - (secondary_tbl%>%pull(divida_principal))[dim(secondary_tbl)[1]] / (secondary_tbl%>%pull(valor_emprestado))[1]
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso > dim(z[[l]])[1]*0.25 & dias_pos_desembolso < dim(z[[l]])[1]*0.5)
            liq_50 <- 1 - (secondary_tbl%>%pull(divida_principal))[dim(secondary_tbl)[1]] / (secondary_tbl%>%pull(valor_emprestado))[1]
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso > dim(z[[l]])[1]*0.5 & dias_pos_desembolso < dim(z[[l]])[1]*0.75)
            liq_75 <- 1 - (secondary_tbl%>%pull(divida_principal))[dim(secondary_tbl)[1]] / (secondary_tbl%>%pull(valor_emprestado))[1]
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso > dim(z[[l]])[1]*0.75)
            liq_dia_90 <- 1 - (secondary_tbl%>%pull(divida_principal))[dim(secondary_tbl)[1]] / (secondary_tbl%>%pull(valor_emprestado))[1]
            
            point_x <- c((z[[l]]%>% pull(id))[1],liq_25, liq_50, liq_75, liq_dia_90)
            db_x_liquidation <- rbind(db_x_liquidation, point_x)
            
        } else if (dim(z[[l]])[1] == 3) {
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso >= dim(z[[l]])[1]*0.25)
            liq_25 <- 1 - (secondary_tbl%>%pull(divida_principal))[1] / (secondary_tbl%>%pull(valor_emprestado))[1]
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso >= dim(z[[l]])[1]*0.5)
            liq_50 <- 1 - (secondary_tbl%>%pull(divida_principal))[1] / (secondary_tbl%>%pull(valor_emprestado))[1]
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso >= dim(z[[l]])[1] - 1)
            liq_dia_90 <- 1 - (secondary_tbl%>%pull(divida_principal))[1] / (secondary_tbl%>%pull(valor_emprestado))[1]
            liq_75 <- liq_dia_90
            
            point_x <- c((z[[l]]%>% pull(id))[1],liq_25, liq_50, liq_75, liq_dia_90)
            db_x_liquidation <- rbind(db_x_liquidation, point_x)
            
        } else if (dim(z[[l]])[1] <= 2) {
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso >= dim(z[[l]])[1] - 1)
            liq_dia_90 <- 1 - (secondary_tbl%>%pull(divida_principal))[1] / (secondary_tbl%>%pull(valor_emprestado))[1]
            liq_75 <- liq_dia_90
            liq_25 <- liq_dia_90
            liq_50 <- liq_dia_90
            
            point_x <- c((z[[l]]%>% pull(id))[1],liq_25, liq_50, liq_75, liq_dia_90)
            db_x_liquidation <- rbind(db_x_liquidation, point_x)
            
        }
        
    }
    
    colnames(db_x_liquidation) <- c("id", "liq_25", "liq_50", "liq_75","liq_dia_90")
    db_x_liquidation <- as_tibble(db_x_liquidation)
    return(db_x_liquidation)
}

#Calculate media_pgto_x

media_pgto <- function(x) {
    
    db_x_media <- NULL
    
    z <- x %>%
        select(id,dias_pos_desembolso, pgto_diario_esperado, pagamento_diario) %>%
        group_by(id) %>%
        group_split()
    
    nov_25 = round(0.25 * 90, 0)
    nov_50 = round(0.50 * 90, 0)
    nov_75 = round(0.75 * 90, 0)
    nov_100 = 90
    
    for (l in 1 : length(z)) {
        
        
        if (dim(z[[l]])[1] > 90) {
        
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso <= nov_25)
            mean_25 <- (secondary_tbl%>%mutate(mean_25 = mean(pagamento_diario))%>%pull(mean_25))[1]
            mean_pp_25 <- mean_25 / as.numeric(((z[[l]]%>%pull(pgto_diario_esperado))[1]))
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso > nov_25 & dias_pos_desembolso <= nov_50)
            mean_50 <- (secondary_tbl%>%mutate(mean_50 = mean(pagamento_diario))%>%pull(mean_50))[1]
            mean_pp_50 <- mean_50 / as.numeric(((z[[l]]%>%pull(pgto_diario_esperado))[1]))
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso > nov_50 & dias_pos_desembolso <= nov_75)
            mean_75 <- (secondary_tbl%>%mutate(mean_75 = mean(pagamento_diario))%>%pull(mean_75))[1]
            mean_pp_75 <- mean_75 / as.numeric(((z[[l]]%>%pull(pgto_diario_esperado))[1]))
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso > nov_75)
            mean_100 <- (secondary_tbl%>%mutate(mean_100 = mean(pagamento_diario))%>%pull(mean_100))[1]
            mean_pp_90_dias <- mean_100 / as.numeric(((z[[l]]%>%pull(pgto_diario_esperado))[1]))
            
            point_x <- c((z[[l]]%>% pull(id))[1],mean_pp_25, mean_pp_50, mean_pp_75, mean_pp_90_dias)
            db_x_media <- rbind(db_x_media, point_x)
        
        } else {
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso <= dim(z[[l]])[1]*0.25)
            mean_25 <- (secondary_tbl%>%mutate(mean_25 = mean(pagamento_diario))%>%pull(mean_25))[1]
            mean_pp_25 <- mean_25 / as.numeric(((z[[l]]%>%pull(pgto_diario_esperado))[1]))
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso > dim(z[[l]])[1]*0.25 & dias_pos_desembolso <= dim(z[[l]])[1]*0.5)
            mean_50 <- (secondary_tbl%>%mutate(mean_50 = mean(pagamento_diario))%>%pull(mean_50))[1]
            mean_pp_50 <- mean_50 / as.numeric(((z[[l]]%>%pull(pgto_diario_esperado))[1]))
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso > dim(z[[l]])[1]*0.5 & dias_pos_desembolso <= dim(z[[l]])[1]*0.75)
            mean_75 <- (secondary_tbl%>%mutate(mean_75 = mean(pagamento_diario))%>%pull(mean_75))[1]
            mean_pp_75 <- mean_75 / as.numeric(((z[[l]]%>%pull(pgto_diario_esperado))[1]))
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso >  dim(z[[l]])[1]*0.75 )
            mean_100 <- (secondary_tbl%>%mutate(mean_100 = mean(pagamento_diario))%>%pull(mean_100))[1]
            mean_pp_90_dias <- mean_100 / as.numeric(((z[[l]]%>%pull(pgto_diario_esperado))[1]))
            
            point_x <- c((z[[l]]%>% pull(id))[1],mean_pp_25, mean_pp_50, mean_pp_75, mean_pp_90_dias)
            db_x_media <- rbind(db_x_media, point_x)
            
        }
    }
    colnames(db_x_media) <- c("id", "media_pgto_25", "media_pgto_50", "media_pgto_75", "media_pgto_90dias")
    db_x_media <- as_tibble(db_x_media)
    return(db_x_media)
}

#Calculate freq_quartil

freq_quartil <- function(x) {
    
    db_freq_x <- NULL
    
    z <- x %>%
        select(id, dias_pos_desembolso, pagamento_diario, amortizacao_principal_diario, transacionado) %>%
        group_by(id) %>%
        group_split()
    
    nov_25 = round(0.25 * 90, 0)
    nov_50 = round(0.50 * 90, 0)
    nov_75 = round(0.75 * 90, 0)
    nov_100 = 90
    
    for (l in 1 : length(z)) {
        
        if (dim(z[[l]])[1] > 90) {
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso <= nov_25)
            freq_25 <- colSums(cbind(secondary_tbl%>%pull(pagamento_diario), secondary_tbl%>%pull(amortizacao_principal_diario),secondary_tbl%>%pull(transacionado)) != 0)
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso > nov_25 & dias_pos_desembolso <= nov_50)
            freq_50 <- colSums(cbind(secondary_tbl%>%pull(pagamento_diario), secondary_tbl%>%pull(amortizacao_principal_diario),secondary_tbl%>%pull(transacionado)) != 0)
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso > nov_50 & dias_pos_desembolso <= nov_75)
            freq_75 <- colSums(cbind(secondary_tbl%>%pull(pagamento_diario), secondary_tbl%>%pull(amortizacao_principal_diario),secondary_tbl%>%pull(transacionado)) != 0)
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso > nov_75)
            freq_90_days <- colSums(cbind(secondary_tbl%>%pull(pagamento_diario), secondary_tbl%>%pull(amortizacao_principal_diario),secondary_tbl%>%pull(transacionado)) != 0)
            
            point_x <- c((z[[l]]%>% pull(id))[1],freq_25, freq_50, freq_75, freq_90_days)
            db_freq_x <- rbind(db_freq_x, point_x)

        
        } else {
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso <= dim(z[[l]])[1]*0.25)
            freq_25 <- colSums(cbind(secondary_tbl%>%pull(pagamento_diario), secondary_tbl%>%pull(amortizacao_principal_diario),secondary_tbl%>%pull(transacionado)) != 0)
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso > dim(z[[l]])[1]*0.25 & dias_pos_desembolso <= dim(z[[l]])[1]*0.50)
            freq_50 <- colSums(cbind(secondary_tbl%>%pull(pagamento_diario), secondary_tbl%>%pull(amortizacao_principal_diario),secondary_tbl%>%pull(transacionado)) != 0)
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso > dim(z[[l]])[1]*0.50 & dias_pos_desembolso <= dim(z[[l]])[1]*0.75)
            freq_75 <- colSums(cbind(secondary_tbl%>%pull(pagamento_diario), secondary_tbl%>%pull(amortizacao_principal_diario),secondary_tbl%>%pull(transacionado)) != 0)
            
            secondary_tbl <- z[[l]]%>%filter(dias_pos_desembolso > dim(z[[l]])[1]*0.75)
            freq_90_days <- colSums(cbind(secondary_tbl%>%pull(pagamento_diario), secondary_tbl%>%pull(amortizacao_principal_diario),secondary_tbl%>%pull(transacionado)) != 0)
            
            point_x <- c((z[[l]]%>% pull(id))[1],freq_25, freq_50, freq_75, freq_90_days)
            db_freq_x <- rbind(db_freq_x, point_x)
            
        }
    }
    colnames(db_freq_x) <- c("id", "freq_25_pgto", "freq_25_amort", "freq_25_trans","freq_50_pgto","freq_50_amort",
                                      "freq_50_trans","freq_75_pgto","freq_75_amort","freq_75_trans", "freq_90_pgto",
                                      "freq_90_amort","freq_90_trans")
    db_freq_x  <- as_tibble(db_freq_x)
    return(db_freq_x)
}

#Calculate diasdepois2_x

diasdepois2 <- function(x, type = "a" | "p" | "t") {
    
    db_diasdepois <- NULL 
    
    if( type == "a") {
        
        z <- x %>%
            select(id, dias_pos_desembolso, amortizacao_principal_diario) %>%
            filter(amortizacao_principal_diario != 0) %>%
            slice_head(n = 1) %>%
            mutate(diasdepois_amort = dias_pos_desembolso)
        
        z <- juros_d2 %>% left_join(z, by = c("id" = "id"))%>%
            mutate(diasdepois_amort = replace_na(diasdepois_amort, "Missed 90 days"))%>%
            select(-dias_pos_desembolso, -amortizacao_principal_diario, - juros_d_nivel)%>%
            group_by(id) %>%
            group_split()
        
    } else if( type == "p") {
        
        z <- x %>%
            select(id, dias_pos_desembolso, pagamento_diario) %>%
            filter(pagamento_diario != 0) %>%
            slice_head(n = 1) %>%
            mutate(diasdepois_pgto = dias_pos_desembolso)
        
        z <- juros_d2 %>% left_join(z, by = c("id" = "id"))%>%
            mutate(diasdepois_pgto = replace_na(diasdepois_pgto, "Missed 90 days"))%>%
            select(-dias_pos_desembolso, -pagamento_diario, - juros_d_nivel)%>%
            group_by(id) %>%
            group_split()
        
    } else if( type == "t") {
        
        z <- x %>%
            select(id, dias_pos_desembolso, transacionado) %>%
            filter(transacionado != 0) %>%
            slice_head(n = 1) %>%
            mutate(diasdepois_trans = dias_pos_desembolso)
        
        z <- juros_d2 %>% left_join(z, by = c("id" = "id"))%>%
            mutate(diasdepois_trans = replace_na(diasdepois_trans, "Missed 90 days"))%>%
            select(-dias_pos_desembolso, -transacionado, - juros_d_nivel)%>%
            group_by(id) %>%
            group_split()
        
    }
    
    for (l in 1 : length(z)) {
        
        dias_x <- c((z[[l]]%>% pull(id))[1],(z[[l]]%>% pull(colnames(z[[l]])[2]))[1])
        db_diasdepois <- rbind(db_diasdepois, dias_x)
        
    }
    colnames(db_diasdepois) <- c("id", colnames(z[[l]])[2])
    db_diasdepois <- as_tibble(db_diasdepois)
    return(db_diasdepois)
}