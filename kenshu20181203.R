########### 2018/12/3 ���C�p

options(digits=10)
options(scipen=100)

#-----���L�͓K�X�C����v��
setwd("C:/Users/fu300/OneDrive/�f�X�N�g�b�v/torihiki")

# install.packages("tidyverse")
library(tidyverse)

fnames <- dir(pattern="^\\d{2}_.*.csv") #�t�@�C�����̎擾
fnames
fnames <-fnames[34:35] #27�Ԗڂ����{

dl <- lapply(fnames, read.csv)
torihiki <- do.call(rbind, dl)
rm(dl)
head(torihiki)
str(torihiki)

col_list <- c("No","���","�n��","code","����","�s��","�n�於","�w","�w��","������z","�ؒP��","�Ԏ��",
              "m2","m2�P��","�y�n�`��","�Ԍ�","�����ʐ�","���z�N","�����\��","�����p�r","����̗��p�ړI","���H����","���H���","���H����",
              "�s�s�v��","���؂���","�e�ϗ�","������_","����","���l" )
names(torihiki) <- col_list   # �񖼂�ύX

num1_4 <- function(x) {
  x <- sub("�P","1",x, fixed=T)
  x <- sub("�Q","2",x, fixed=T)
  x <- sub("�R","3",x, fixed=T)
  x <- sub("�S","4",x, fixed=T)
  return(x)
} 
torihiki$������_ <- as.factor(torihiki$������_)
levels(torihiki$������_) <- num1_4(levels(torihiki$������_))  # �S�p�����𔼊p�� 
torihiki$jiten <- as.integer(substr(torihiki[,28],3,4))+1988-.125+as.numeric(substr(torihiki[,28],7,7))*.25
torihiki$qtr <- paste(as.integer(substr(torihiki[,28],3,4))+1988,"Q",as.numeric(substr(torihiki[,28],7,7)),sep = "")
torihiki$tori_year <- as.integer(torihiki$jiten)
kami <- function(x) { ifelse(log10(x)<2, round(x, digits =0), signif(x,digits=2)) }
torihiki$����m2 <- with( torihiki,ifelse(m2 == "2000�u�ȏ�" & ��� == "��n(�y�n)",
                                       kami(������z/as.numeric(as.character(m2�P��))),as.numeric(as.character(m2))))
torihiki$����m2�P�� <- with( torihiki, ifelse( ��� == "���Ã}���V������", kami(������z/as.numeric(as.character(m2))),
                                          ifelse( (���=="�_�n" | ��� == "�ђn") & m2 != "5000�u�ȏ�",kami(������z/as.numeric(as.character(m2))),as.numeric(m2�P��))))
torihiki$���z�N <- as.character(torihiki$���z�N)
torihiki$build_year <- with( torihiki, ifelse( ���z�N=="��O",1945,
                                              ifelse(���z�N=="�������N",1989,
                                                        ifelse(substr(���z�N,1,2)=="���a", as.integer(substr(���z�N,3,nchar(���z�N)-1))+1925,
                                                               ifelse(substr(���z�N,1,2)=="����", as.integer(substr(���z�N,3,nchar(���z�N)-1))+1988,NA)))))
torihiki$chikugo_year <- with(torihiki,round(jiten-build_year,digits = 0))
torihiki$eki_hun <- with(torihiki,as.integer(as.character(�w��))) #30���ȏ��NA
torihiki$yuka.m2 <- as.integer(as.character(torihiki$�����ʐ�))
torihiki <- droplevels(torihiki)
torihiki$�n��  <- torihiki$�n�� %>% factor(levels=c("�Z��n", "���ƒn", "�H�ƒn", "��n�����n"))

torihiki$kibo <- cut(torihiki$����m2,breaks=c(0,200,500,1000,5000,10000,30000, 10000000), right=FALSE)
levels(torihiki$kibo) <- c("200����","500����" ,"1000����","5000����","10000����","30000����","30000�ȏ�")

##### ����i�荞��

# ��|�s�̎�����z�̕���
otake <- torihiki %>% subset(�s��=="��|�s") %>% droplevels()
table(otake$���)
otake %>% ggplot(aes(x=jiten, y=������z)) + 
  geom_point(aes(color=���, shape=�n�� ), size=5, alpha=0.8) +scale_shape(solid = TRUE) + theme_bw() +
  ggtitle("��|�s�̎�ށE�n��ʁA������z�E���_plot")
otake %>% ggplot(aes(x=jiten, y=������z)) + 
  geom_point( size=3, alpha=0.3) + facet_grid(���~�n��)+theme_bw() +
  ggtitle("��|�s�̎�ށE�n��ʁA������z�E���_plot")
otake_over_1oku <- otake %>% subset(������z>=100000000)
write.csv(otake_over_1oku, "otake_over_1oku.csv")
table(otake$���, otake$�n��)

otake %>% subset(���=="��n(�y�n)") %>% ggplot(aes(x=jiten,y=����m2�P��)) +
  geom_point(size=2, alpha=0.3) + facet_grid(�n��~kibo)

##### �L�����E�R����

### ��|�s�Ǝ��ӎs���i�����s�s�ƎR�����⍑�s�E��όS�a�ؒ�
shuhen <- torihiki %>% subset(�s��=="��|�s" | �s��=="�����s�s" | �s��=="�⍑�s" | �s��=="��όS�a�ؒ�")
shuhen <- shuhen %>% droplevels()
table(shuhen$�s��, shuhen$���)
shuhen %>% ggplot(aes(x=jiten, y=������z)) + 
  geom_point(aes(shape=���), size=3, alpha=0.4) +
  ggtitle("��|�s���ӂ̎�ޕʁA������z�E���_plot")
# �⍑�s��170�������O ���20���Ŏs�E��މx
shuhen %>% ggplot(aes(x=jiten, y=������z)) + 
  geom_point( aes(color=�n��), size=3, alpha=0.5) + facet_grid(���~�s��)+ylim(0,2000000000) +theme_bw() +
  ggtitle("��|�s���ӂ̎�ޕʁA������z�E���_plot   �⍑�s���c��170���~����")

# 1���~�ȏ�
shuhen_1oku <- shuhen %>% subset(������z >= 100000000)
write.csv(shuhen_1oku, "shuhen_1oku.csv")  

### ���ӂŁA�K�͂ƒP��
shuhen_tochi <- shuhen %>% subset(���=="��n(�y�n)")

shuhen_tochi %>% ggplot(aes(jiten, m2�P�� )) + geom_point( aes(shape=�s��),size=2, alpha=0.3) + 
  geom_smooth(se=FALSE) + facet_grid(�n��~kibo) + theme_bw()+ ggtitle("��|�s���ӂ̗p�r�E�K�͕ʂ̎��_�Em2�P���v���b�g")

shuhen_tochi_1man <- shuhen_tochi %>% subset(����m2 >= 10000)

shuhen_tochi_1man %>% ggplot(aes(jiten, m2�P��, shape=�s��, color=�n�� )) + geom_point(size=5, alpha=1) + 
  theme_bw()+ ggtitle("��|�s���ӂ�1��m2�ȏ�̎��_�Em2�P���v���b�g")
shuhen_tochi_1man %>% ggplot(aes(jiten, m2�P��, shape=�n��, color=�n�� )) + geom_point(size=5, alpha=1) + 
  theme_bw()+facet_grid(�s��~.)  +ggtitle("��|�s���ӂ�1��m2�ȏ�̎��_�Em2�P���v���b�g")

### 2���S��
# ���z
torihiki %>% ggplot(aes(x=jiten,y=������z)) + geom_point()
torihiki %>% ggplot(aes(x=jiten,y=������z)) + geom_point(size=2.5, alpha=0.3)+facet_grid(����~���)

# m2�P��
tochi <- torihiki %>% subset(���=="��n(�y�n)")

tochi %>% ggplot(aes(jiten, m2�P�� )) + geom_point(size=2, alpha=0.3) + 
  geom_smooth(se=FALSE) + facet_grid(�n��~kibo) + theme_bw()+ ggtitle("�L�����ƎR�����̗p�r�E�K�͕ʂ̎��_�Em2�P���v���b�g")

tochi %>% subset(����m2 >= 30000) %>% 
  ggplot(aes(jiten, m2�P�� )) + geom_point(size=2, alpha=0.3)+ # ylim(0,200000) + 
  geom_smooth(se=FALSE) + facet_grid(�n��~����) +theme_bw() + ggtitle("���E�p�r�ʂ̎��_�Em2�P���v���b�g(3��m2�ȏ�) ")

table(otake$����m2)
table(otake[str_detect(otake$���,"��n"),]$����m2 )


#############################################
######  ���{  ###��������S���ʂ�plot
###  �t�@�C�����̎擾 
fnames <- dir(pattern="^\\d{2}_.+_\\d{5}_\\d{5}.csv")  #�t�@�C�����̎擾
fnames <- fnames[c(27)]  # c( )���ɐ���  ���s�ƕ���c(26, 28) �O�d�Ƒ��`�a�̎Rc(24,27:30)
fnames

### �t�@�C���̌���
dl <- lapply(fnames, read.csv,encoding = "Shift_JIS",na.strings = c("","NULL"))
torihiki <- do.call(rbind, dl)
rm(dl)

col_list <- c("No","���","�n��","code","����","�s��","�n�於","�w","�w��","������z","�ؒP��","�Ԏ��",
              "m2","m2�P��","�y�n�`��","�Ԍ�","�����ʐ�","���z�N","�����\��","�����p�r","����̗��p�ړI","���H����","���H���","���H����",
              "�s�s�v��","���؂���","�e�ϗ�","������_","����","���l" )
names(torihiki) <- col_list   # �񖼂�ύX

num1_4 <- function(x) {
  x <- sub("�P","1",x, fixed=T)
  x <- sub("�Q","2",x, fixed=T)
  x <- sub("�R","3",x, fixed=T)
  x <- sub("�S","4",x, fixed=T)
  return(x)
} 
torihiki$������_ <- as.factor(torihiki$������_)
levels(torihiki$������_) <- num1_4(levels(torihiki$������_))  # �S�p�����𔼊p�� 
torihiki$jiten <- as.integer(substr(torihiki[,28],3,4))+1988-.125+as.numeric(substr(torihiki[,28],7,7))*.25
torihiki$qtr <- paste(as.integer(substr(torihiki[,28],3,4))+1988,"Q",as.numeric(substr(torihiki[,28],7,7)),sep = "")
torihiki$tori_year <- as.integer(torihiki$jiten)
kami <- function(x) { ifelse(log10(x)<2, round(x, digits =0), signif(x,digits=2)) }
torihiki$����m2 <- with( torihiki,ifelse(m2 == "2000�u�ȏ�" & ��� == "��n(�y�n)",
                                       kami(������z/as.numeric(as.character(m2�P��))),as.numeric(as.character(m2))))
torihiki$����m2�P�� <- with( torihiki, ifelse( ��� == "���Ã}���V������", kami(������z/as.numeric(as.character(m2))),
                                          ifelse( (���=="�_�n" | ��� == "�ђn") & m2 != "5000�u�ȏ�",kami(������z/as.numeric(as.character(m2))),as.numeric(m2�P��))))
torihiki$���z�N <- as.character(torihiki$���z�N)
torihiki$build_year <- with( torihiki, ifelse( ���z�N=="��O",1945,
                                              ifelse(���z�N=="�������N",1989,
                                                        ifelse(substr(���z�N,1,2)=="���a", as.integer(substr(���z�N,3,nchar(���z�N)-1))+1925,
                                                               ifelse(substr(���z�N,1,2)=="����", as.integer(substr(���z�N,3,nchar(���z�N)-1))+1988,NA)))))
torihiki$chikugo_year <- with(torihiki,round(jiten-build_year,digits = 0))
torihiki$eki_hun <- with(torihiki,as.integer(as.character(�w��))) #30���ȏ��NA
torihiki$yuka.m2 <- as.integer(as.character(torihiki$�����ʐ�))
torihiki$�s�n�� <- paste0(torihiki$�s��, torihiki$�n�於)
torihiki <- droplevels(torihiki)
torihiki$�n��  <- torihiki$�n�� %>% factor(levels=c("�Z��n", "���ƒn", "�H�ƒn", "��n�����n"))

torihiki$city <- as.character(torihiki$�s��)
torihiki$city[str_detect(torihiki$�s��,"^���s")] <- "���s"
torihiki$city[str_detect(torihiki$�s��,"��s")] <- "��s"
torihiki$city <- as.factor(torihiki$city)

### �i�荞��
city_n <- torihiki %>% group_by(�s��) %>% summarise(
            chiku_n=length(unique(�s�n��)),
            eki_n = length(unique(�w)) )
torihiki %>% subset(�s��=="���s") %>% ggplot(aes(x=jiten, y=������z)) + 
  geom_point(aes(color=�n��,shape=���),size=3, alpha=0.5) +
  ggtitle("���_�E������zplot")  


torihiki %>% subset(�s��=="���s") %>% ggplot(aes(x=jiten, y=������z)) + 
  geom_point(aes(color=�n��,shape=���),size=3, alpha=0.5) +
  ggtitle("�n�斈�̎��_�E������zplot")  + facet_wrap(�n�於~.)

torihiki %>% subset(�s��=="���s�铌��" & ���=="��n(�y�n)") %>% ggplot(aes(x=jiten, y=������z)) + 
  geom_point(aes(color=�n��,shape=�n��),size=3, alpha=0.5) +
  ggtitle("�y�n�̒n�斈�̎��_�Em2�P��plot")  + facet_wrap(�n�於~.)



torihiki %>% subset(�s��=="��͓��S�͓쒬") %>% ggplot(aes(x=jiten, y=m2�P��)) + 
  geom_point(aes(color=�n��,shape=���),size=3, alpha=0.5) +
  ggtitle("�y�n�̒n�斈�̎��_�Em2�P��plot")  + facet_wrap(�n�於~.)
torihiki %>% subset(�s��=="�����s") %>% ggplot(aes(x=jiten, y=m2�P��)) + 
  geom_point(aes(color=�n��,shape=���),size=3, alpha=0.5) +
  ggtitle("�y�n�̒n�斈�̎��_�Em2�P��plot")  + facet_wrap(�w~.)




#======  �����s�̍X�n�A�Z��E���ƁE�H�Ƃ̋K�͉x�P��
hosaka <- torihiki[torihiki$�s��=="�����s",]
hosaka_tochi <- hosaka[hosaka$��� == "��n(�y�n)",]                            
table(hosaka_tochi$�n��)                          
#��n�����n�͏��Ȃ��̂ŏ��O
hosaka_tochi <- hosaka_tochi[hosaka_tochi$�n�� != "��n�����n",]
hosaka_tochi <- droplevels(hosaka_tochi)
# �n��factor���Z��E���ƁE�H�Ƃ̏��ɕ��т�����
hosaka_tochi$�n�� <- fct_relevel(hosaka_tochi$�n��,c("�Z��n","���ƒn","�H�ƒn"))

# cut�֐����g���āA�n�ς��敪(right=TRUE���ƗႦ��100��200�̊ԂȂ�A100�ȏ�200�����AFALSE����100��200���ȉ��ƂȂ�)
hosaka_tochi$kibo <- cut(hosaka_tochi$����m2,breaks=c(0,100,250,500,1000,5000,100000), right=FALSE)
levels(hosaka_tochi$kibo) <- c("100����","100�ȏ�250����","250�ȏ�500����","500�ȏ�1000����","1000�ȏ�5000����","5000�ȏ�")
table(hosaka_tochi$kibo)

#####�`��
# �S�̂̋K�͕�
hosaka_tochi %>% ggplot(aes(jiten,m2�P��)) + geom_point(size=2, alpha=0.5)+geom_smooth() +  facet_wrap(kibo~.)
# �K�͂ƒn���
hosaka_tochi %>% ggplot(aes(jiten,m2�P��)) + geom_point(size=2, alpha=0.5)+geom_smooth() +  facet_grid(kibo~�n��)
hosaka_tochi %>% ggplot(aes(jiten,m2�P��)) + geom_point(size=2, alpha=0.5)+geom_smooth() +  facet_grid(�n��~kibo)

#��L�̃T���v����
aggregate(kibo ~ �n��, data=hosaka_tochi, table)

#=================  �H�ƒn�E�X�n�݂̂ŒP���E���_�̃O���t���쐬
osaka_kogyo <- torihiki[torihiki$�n��=="�H�ƒn" & torihiki$���=="��n(�y�n)",]
osaka_kogyo$city %>% table() %>% sort() %>% tail()
# ���s�A�����s�A��s�̃f�[�^�ɍi�荞��
osaka3_kogyo <- osaka_kogyo[str_detect(osaka_kogyo$city,"���s") | str_detect(osaka_kogyo$city,"��s"),]
osaka3_kogyo <- osaka3_kogyo %>% droplevels()


# cut�֐����g���āA�n�ς��敪(right=TRUE���ƗႦ��100��200�̊ԂȂ�A100�ȏ�200�����AFALSE����100��200���ȉ��ƂȂ�)
# �T���v�����ɉ�����cut�֐���breaks=c(  )����K�X�C��, ������level�̐��▼�̂��C��
osaka3_kogyo$kibo <- cut(osaka3_kogyo$����m2,breaks=c(0,100,250,500,1000,5000,100000), right=FALSE)
levels(osaka3_kogyo$kibo) <- c("100����","100�ȏ�250����","250�ȏ�500����","500�ȏ�1000����","1000�ȏ�5000����","5000�ȏ�")

osaka3_kogyo$city <- fct_relevel(osaka3_kogyo$city, c("���s", "��s", "�����s"))
aggregate(data=osaka3_kogyo,kibo~city,table)


#---�O���t�`��
# �N���Ƃ̔��Ђ��}
ggplot(data=osaka3_kogyo) + geom_boxplot(aes(x=as.factor(tori_year), y=m2�P��))
# �l�����ƒP���̎U�z�}
ggplot(data=osaka3_kogyo,aes(x=jiten, y=m2�P��)) + geom_point(aes(alpha=0.5))+geom_smooth()
# ���ɂ����̂ŁA�K�͕ʂɕ`��
osaka3_kogyo %>% ggplot(aes(jiten, m2�P�� )) + geom_hline(yintercept=c(100000,200000),size=0.3) +
  geom_point(size=2, alpha=0.3) + geom_smooth() + facet_grid(city~kibo) + ggtitle("���s�E��s�E�����s�̍H�ƒn  �K�͕� �y�nm2�P��")

# �e�[�}�ύX
osaka3_kogyo %>% ggplot(aes(jiten, m2�P�� )) + geom_point(size=2, alpha=0.3) + geom_smooth() + facet_grid(city~kibo) + theme_bw()
