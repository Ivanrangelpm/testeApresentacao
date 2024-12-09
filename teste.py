import psutil, time, mysql.connector, platform, pandas as pd,  dotenv , os, requests,  json
from socket import gethostname
from datetime import datetime, timedelta
dotenv.load_dotenv(dotenv.find_dotenv())
senhaBD = os.getenv("SENHA_DB")

i = 0
nomeMaquina = gethostname()  
sistemaOperacional = platform.system()



db_connection = mysql.connector.connect(
   host='10.18.35.41', user='root', password=senhaBD, database='TagTech'
   )
cursor = db_connection.cursor()


baseurl = os.getenv("URL")
url = f'{baseurl}//rest/api/2/issue'
token = os.getenv("TOKEN")
email = os.getenv("LOGIN")

ultimoChamadoCPU = datetime.now() - timedelta(hours=1)
ultimoChamadoRAM = datetime.now() - timedelta(hours=1)
ultimoChamadoDisco = datetime.now() - timedelta(hours=1)





def mandarAlertaJira(componente, numPico):
   data = pd.Timestamp.now()
   data = data.replace(microsecond=0)
   headers = {
   "Accept": "application/json",
   "Content-Type": "application/json"
   }


   payload=json.dumps(
       {
           "fields":{
               "project":
                   {
                       "key":"TTCS"
                   },
                   "summary": "pico de uso do componente : {} as {}".format(componente, data),
                   "description": "O componente {} teve um pico de uso de ´´{} por cento as **{} no servidor ''{}".format(componente,numPico, data, nomeMaquina),
                   "issuetype":{
                       "name":"Support"
                   }
           }
       }
   )



   response = requests.post(url,headers=headers,data=payload,auth=(email,token))


   print(response.text)


while True:

   if(sistemaOperacional=="Windows"):
       disco = psutil.disk_usage('C:\\')
   elif(sistemaOperacional=="Linux"):
       disco = psutil.disk_usage('/')

   usoDeCPU = psutil.cpu_percent(interval=1)
   freqDeCPU = psutil.cpu_freq()
   freqDeCPU = freqDeCPU.current
   
   memRAM = psutil.virtual_memory()
   ramGigaBites = memRAM.used / 1024 ** 3
   percentRAM = memRAM.percent 
   discoUsado = disco.used
   percentDisco = disco.percent
   hora_atual = datetime.now()
  
   if usoDeCPU >= 80 and hora_atual - ultimoChamadoCPU >= timedelta(hours=1): 
       mandarAlertaJira("CPU", usoDeCPU)
       ultimoChamadoCPU = hora_atual
   if percentRAM >= 80 and hora_atual - ultimoChamadoRAM >= timedelta(hours=1):
       mandarAlertaJira("memória RAM", percentRAM)
       ultimoChamadoRAM = hora_atual
   if percentDisco >= 80 and hora_atual - ultimoChamadoDisco >= timedelta(hours=1):
       mandarAlertaJira("disco", percentDisco)
       ultimoChamadoDisco = hora_atual
   
   
   
   i = i + 1

   sql = "INSERT INTO registros ( percentualMemoria, gigaBytesMemoria, qtdUtilizadaDisco, percentualDisco, percentualCPU, frequenciaCPU, fkMaquina) VALUES (%s, %s, %s, %s, %s, %s, 3)"
   values = (percentRAM, ramGigaBites, discoUsado, percentDisco, usoDeCPU, freqDeCPU)
   cursor.execute(sql, values)
   db_connection.commit()

   time.sleep(5)

   
   print("##############################################################################################")

