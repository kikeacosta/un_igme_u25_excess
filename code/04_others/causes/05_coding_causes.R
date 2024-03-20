# definition of causes of death according to Li Liu 
# (https://www.thelancet.com/cms/10.1016/S2352-4642(21)00311-4/attachment/0709183d-da83-46e8-8446-ef2a2620f0f0/mmc1.pdf)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


{
  b1 <- c(paste0("B", sprintf("%02d", 20:24)))
  b2 <- c(paste0("A", 00:09))
  b3 <- c('A37')
  b4 <- c(paste0("A", sprintf("%02d", 33:35)))
  b5 <- c('B05')
  b6 <- c('A39', paste0("A", 83:87), 'G00', 'G03', 'G04')
  b7 <- c(paste0("B", sprintf("%02d", 50:54)), 'P373', 'P374') ####
  b8 <- c(paste0("H", sprintf("%02d", 65:66)), 
          paste0("J", sprintf("%02d", 00:22)), 
          'J85', 'P23')
  b9 <- c('P010', 'P011', 'P07', 'P22', paste0("P", sprintf("%02d", 25:28)),
          'P612', 'P77')
  b10 <- c(paste0("P", sprintf("%03d", 017:021)), 
           paste0("P", sprintf("%03d", 024:026)),
           'P03',
           paste0("P", sprintf("%02d", 10:15)),
           paste0("P", sprintf("%02d", 20:21)),
           'P24',
           'P50',
           'P90', 'P91')
  b11 <- paste0("P", sprintf("%02d", 35:39))
  b12 <- paste0("Q", sprintf("%02d", 00:99))
  
  b12 <- c('C23', 'C24')
  b13 <- 'C25'
  b14 <- 'C32'
  b15 <- c('C33', 'C34')
  b16 <- c('C43', 'C44')
  b17 <- 'C50'
  b18 <- c(paste0("C", sprintf("%02d", 53:55)))
  b19 <- 'C56'
  b20 <- 'C61'
  b21 <- 'C64'
  b22 <- 'C67'
  b23 <- 'C71'
  b24 <- c(paste0("C", 81:96))
  b25 <- c(paste0("D", sprintf("%02d",00:48)))
  b26 <- c(paste0("E", sprintf("%02d",10:14)))
  b27 <- c(paste0("D", 50:53), paste0("E", 40:64))
  b28 <- c(paste0("E", sprintf("%02d",86:87)))
  b29 <- c('F01', 'F03', 'G30')
  b30 <- c(paste0("F", sprintf("%02d",10:19)))
  b31 <- 'G20'
  b32 <- c('G40', 'G41')
  b33 <- c(paste0("I", sprintf("%02d",05:09)))
  b34 <- c(paste0("I", sprintf("%02d",10:15)))
  b35 <- c(paste0("I", sprintf("%02d",20:25)))
  b36 <- c(paste0("I", sprintf("%02d",26:28)))
  b37 <- c(paste0("I", sprintf("%02d",34:38)))
  b38 <- 'I42'
  b39 <- 'I46'
  b40 <- c(paste0("I", sprintf("%02d",47:49)))
  b41 <- c(paste0("I", 50:51))
  b42 <- c(paste0("I", 60:69))
  b43 <- 'I70'
  b44 <- 'I71'
  b45 <- c(paste0("J", sprintf("%02d",00:06)), paste0("J", 20:22))
  b46 <- c(paste0("J", 10:18))
  b47 <- c(paste0("J", 40:47))
  b48 <- c(paste0("J", sprintf("%02d",80:84)))
  b49 <- c(paste0("J", 96))
  b50 <- c(paste0("K", sprintf("%02d",35:46)), 'K56')
  b51 <- c(paste0("K", sprintf("%02d",70:76)))
  b52 <- c(paste0("M", sprintf("%02d",00:99)))
  b53 <- c(paste0("N", sprintf("%02d",00:39)))
  b54 <- c(paste0("O", sprintf("%02d",00:99)))
  b55 <- c(paste0("P", sprintf("%02d",00:96)))
  b56 <- c(paste0("Q", sprintf("%02d",00:99)))
  b57 <- c(paste0("V", sprintf("%02d",01:89)))
  b58 <- c(paste0("W", sprintf("%02d",01:19)))
  b59 <- c(paste0("W", sprintf("%02d",32:34)))
  b60 <- c(paste0("W", 65:74))
  b61 <- c(paste0("W", 75:84))
  b62 <- 'X40-X49'
  b63 <- 'X60-X84'
  b64 <- 'X85-Y09'
  b65 <- 'Y10-Y34'
  b99 <- c(paste0("R", sprintf("%02d",00:08)), paste0("R", sprintf("%02d",10:99)))
  bcv <- c("U07")
  crs <- 'R09'
  inj <- c(paste0("S", sprintf("%02d",00:99)), paste0("T", sprintf("%02d",00:99)))
  b73 <- c(paste0("P", sprintf("%02d",00:96)))
}
