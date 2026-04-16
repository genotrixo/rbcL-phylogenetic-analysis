# 1. تحميل المكتبات
library(ggtree)
library(ggplot2)
library(ape)

# 2. بيانات الشجرة الأصلية
tree_data <- "(((((((((((('NC 035143.1:55036-55500':0.0,'NC 079695.1:55086-55550':0.0)0.0730[stddev=0.80684571]:0.0,'NC 051544.1:54990-55454':0.0)0.0170[stddev=0.31464265]:0.0,'NC 057196.1:55094-55558':0.0)0.0000[stddev=0.00000000]:0.0,'NC 082140.1:54994-55458':0.0)0.0090[stddev=0.00000000]:0.0,Ocimum_basilicum:0.0)0.0540[stddev=0.68920244]:0.0,('NC 060460.1:123424-123888':0.0,'NC 043873.1:54509-54973':0.0)0.8020[stddev=1.26491106]:0.00293244814967326)0.7780[stddev=1.33078924]:0.00292686752642634,('NC 056915.1:55587-56051':0.0,('NC 067688.1:55243-55707':0.0,('NC 067663.1:55250-55714':0.0,('NC 056915.1:55527-56051':0.0,'NC 067688.1:55183-55707':0.0)0.0300[stddev=0.53944416]:0.0)0.0080[stddev=0.00000000]:0.0)0.0190[stddev=0.31464265]:0.0)0.1730[stddev=1.18785521]:0.000182748538011696)0.5420[stddev=1.57607106]:0.00293236149132896,('NC 067709.1:55120-55644':0.0,('NC 067702.1:55174-55698':0.0,'NC 067679.1:55151-55675':0.0)0.7970[stddev=1.28802174]:0.00292712684331549)0.0700[stddev=0.80684571]:0.0)0.0250[stddev=0.44271887]:0.0,'NC 057679.1:55175-55699':0.00293244823720995)0.1850[stddev=1.21490740]:0.00157133786974059,('NC 030757.1:55241-55765':0.0,'NC 030755.1:55192-55716':0.0)0.7100[stddev=1.43492160]:0.00157699738010735)0.5200[stddev=1.57987341]:0.00271489170421083,(Thymus_capitatus:0.0,'NC 069583.1:55036-55560':0.0029324481645139)0.5760[stddev=1.56556699]:0.00293828190534612)0.5160[stddev=1.58082257]:0.00293208758214044,('NC 064128.1:54734-55258':0.00293402137398714,(Mentha_longifolia:0.0,('NC 037247.1:54995-55381':0.0,('AY570426.1:50-436':0.0,('NC 050900.1:54427-54813':0.0,('NC 086489.1:55106-55492':0.0,('NC 032054.1:54974-55360':0.0,('NC 050943.1:54867-55253':0.0,('NC 081477.1:54942-55328':0.0,('NC 071750.1:54942-55328':0.0,('NC 085322.1:55112-55498':0.0,'NC 084048.1:56357-56743':0.0)0.0510[stddev=0.68920244]:0.0)0.0000[stddev=0.00000000]:0.0)0.0000[stddev=0.00000000]:0.0)0.0000[stddev=0.00000000]:0.0)0.0000[stddev=0.00000000]:0.0)0.0000[stddev=0.00000000]:0.0)0.0000[stddev=0.00000000]:0.0)0.0000[stddev=0.00000000]:0.0)0.0090[stddev=0.00000000]:0.0)0.9870[stddev=0.44271887]:0.0118036392720135)0.7430[stddev=1.38708327]:0.00293820379328102,Spathodea_campanulata:0.026932498274339);"

my_tree <- read.tree(text = tree_data)

# 3. تنظيف الأسماء
my_tree$tip.label <- gsub("'", "", my_tree$tip.label)
my_tree$tip.label <- gsub(" ", "_", my_tree$tip.label)

# القاموس
name_mapping <- c(
  "NC_035143.1:55036-55500" = "Ocimum basilicum (2)",
  "NC_079695.1:55086-55550" = "Ocimum americanum",
  "NC_051544.1:54990-55454" = "Nepeta cataria",
  "NC_057196.1:55094-55558" = "Ocimum gratissimum",
  "NC_082140.1:54994-55458" = "Ocimum x africanum",
  "Ocimum_basilicum"        = "Ocimum basilicum (1)",
  "NC_060460.1:123424-123888" = "Ocimum kilimandscharicum",
  "NC_043873.1:54509-54973" = "Ocimum tenuiflorum",
  "NC_056915.1:55587-56051" = "Hanceola exserta (1)",
  "NC_067688.1:55243-55707" = "Isodon wardii (1)",
  "NC_067663.1:55250-55714" = "Isodon irroratus",
  "NC_056915.1:55527-56051" = "Hanceola exserta (2)",
  "NC_067688.1:55183-55707" = "Isodon wardii (2)",
  "NC_067709.1:55120-55644" = "Isodon ramosissimus",
  "NC_067702.1:55174-55698" = "Isodon setschwanensis",
  "NC_067679.1:55151-55675" = "Isodon rugosiformis",
  "NC_057679.1:55175-55699" = "Plectranthus scutellarioides",
  "NC_030757.1:55241-55765" = "Perilla frutescens var. hirtella",
  "NC_030755.1:55192-55716" = "Perilla frutescens var. viridis",
  "Thymus_capitatus"        = "Thymus capitatus",
  "NC_069583.1:55036-55560" = "Coleus hadiensis",
  "NC_064128.1:54734-55258" = "Lycopus europaeus",
  "Mentha_longifolia"       = "Mentha longifolia (1)",
  "NC_037247.1:54995-55381" = "Mentha spicata",
  "AY570426.1:50-436"       = "Salvia lyrata (1)",
  "NC_050900.1:54427-54813" = "Salvia sclarea",
  "NC_086489.1:55106-55492" = "Mentha pulegium",
  "NC_032054.1:54974-55360" = "Mentha longifolia (2)",
  "NC_050943.1:54867-55253" = "Clinopodium chinense",
  "NC_081477.1:54942-55328" = "Thymus serpyllum",
  "NC_071750.1:54942-55328" = "Mentha x villosa",
  "NC_085322.1:55112-55498" = "Clinopodium barosmum",
  "NC_084048.1:56357-56743" = "Salvia lyrata (2)",
  "Spathodea_campanulata"   = "Spathodea campanulata (Outgroup)"
)

mapped_labels <- name_mapping[my_tree$tip.label]
mapped_labels[is.na(mapped_labels)] <- my_tree$tip.label[is.na(mapped_labels)]
my_tree$tip.label <- unname(mapped_labels)
my_tree <- ladderize(my_tree)

# 4. الرسم المحدث لإظهار تباين الألوان وفروق المسافات التطورية بوضوح
p_circular_gradient <- ggtree(my_tree, layout = "circular", size = 1.1, aes(color = branch.length)) + 
  
  # استخدام تدرج ألوان قوي جداً + تحويل المقياس عشان الفروق الدقيقة تبان
  scale_color_gradientn(
    name = "Evolutionary Distance", 
    colors = c("#000080", "#00BFFF", "#32CD32", "#FFD700", "#FF0000"), # كحلي -> سماوي -> أخضر -> أصفر -> أحمر
    trans = "sqrt" # دي الخطوة اللي هتوزع الألوان صح وتلغي تأثير الفرع الشاذ
  ) +
  
  geom_tiplab(
    aes(color = branch.length), 
    size = 3.5,                 
    family = "serif",           
    fontface = "bold.italic",   
    offset = 0.002,             
    show.legend = FALSE         
  ) +
  
  # --- الجزئية الجديدة التي تم إضافتها لتمييز الأنواع ---
  geom_tippoint(
    data = td_filter(label %in% c("Ocimum basilicum (1)", 
                                  "Mentha longifolia (1)", 
                                  "Thymus capitatus")),
    shape = 8,
    size  = 2.5,
    color = "gold",
    stroke = 1.5
  ) +
  # -----------------------------------------------------

scale_x_continuous(expand = expansion(mult = c(0.6, 0.01))) + 
  
  theme_tree() + 
  theme(
    legend.position = "bottom", 
    legend.direction = "horizontal",
    legend.title = element_text(face = "bold", size = 11, vjust = 1),
    legend.text = element_text(size = 9),
    legend.key.width = unit(1.5, "cm"), 
    
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(b = 20)),
    plot.margin = margin(20, 20, 20, 20) 
  )

# عرض الصورة
print(p_circular_gradient)

# حفظ الصورة بأعلى جودة (PNG)
ggsave(
  filename = "My_High_Quality_Plot.png", # اسم الملف
  plot = p_circular_gradient,            # اسم متغير الرسمة بتاعتك
  width = 15,                            # العرض
  height = 12,                           # الطول
  units = "in",                          # وحدة القياس (بوصة)
  dpi = 600,                             # جودة عالية جداً (ممكن تخليها 1200 لو للطباعة الدقيقة)
  bg = "white"                           # خلفية بيضاء عشان تضمن إنها متطلعش شفافة
)