source('https://raw.githubusercontent.com/zeyadissa/open_health_data/main/src/functions.R')
source('https://raw.githubusercontent.com/zeyadissa/open_health_data/main/const/global_var.R')
source('https://raw.githubusercontent.com/zeyadissa/open_health_data/main/src/ae.R')

thf<-'#dd0031'

plot_data <- FINAL_ae_data |> 
  mutate(date = make_date(year=period_year,month=period_month,day=1L)) |> 
  group_by(date) |> 
  filter(period_year >=2019) |> 
  mutate(remergency = remergency_type_1+remergency_type_2+remergency_type_3,
         breaches = remergency_breaches_type_1+remergency_breaches_type_2+remergency_breaches_type_3,
         admissions = remergency_admissions_type_1+remergency_admissions_type_2+remergency_admissions_type_3) |> 
  summarise(remergency = sum(remergency),
            breaches = sum(breaches),
            admissions = sum(admissions))  |> 
  mutate(prop_admission = admissions/remergency,
         prop_breach = breaches / remergency)

attend_plot <- ggplot() +
  geom_line(data=plot_data,aes(x=date,y=remergency/1e6),col=thf,linewidth=1)+
  xlab('') +
  ylab('A&E Attendances (mil)')+
  ggtitle('A&E Attendances',
          subtitle ='Total A&E attendances (mil) per month across England')+
  theme_bw()+
  theme(text = element_text(size = 16))

admit_plot <- ggplot() +
  geom_line(data=plot_data,aes(x=date,y=prop_admission),col=thf,linewidth=1)+
  xlab('') +
  ylab('Proportion admitted (%)')+
  ggtitle('A&E Admissions',
          subtitle ='Proportion (%) of A&E attendances admitted per month')+
  theme_bw()+
  theme(text = element_text(size = 16)) +
  scale_y_continuous(labels=scales::percent,limits=c(0,0.4)) 

breach_plot <- ggplot() +
  geom_line(data=plot_data,aes(x=date,y=prop_breach),col=thf,linewidth=1)+
  xlab('') +
  ylab('Proportion breaches (%)')+
  ggtitle('A&E Breaches',
          subtitle ='Proportion (%) of A&E attendances that breach per month')+
  theme_bw()+
  theme(text = element_text(size = 16)) +
  scale_y_continuous(labels=scales::percent,limits=c(0,0.4)) 

ggsave(admit_plot,filename='output/admit_plot.png')
ggsave(attend_plot,filename='output/attend_plot.png')
ggsave(breach_plot,filename='output/breach_plot.png')
