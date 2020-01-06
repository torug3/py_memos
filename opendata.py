#!/usr/bin/env python
# coding: utf-8

# In[1]:


with open("output.txt", mode="r",encoding="CP932") as f:
    question = f.read()


# In[2]:


import re
import pickle


# In[3]:


question = re.sub("[｜　 \n「」]","",question)   #全角半角スペース、❘、「」、改行の削除


# In[4]:


question = re.sub("特になし","",question) 


# In[5]:


separator = "。"


# In[6]:


question_list = question.split(separator) #セパレータを使って文書をリストに分割


# In[7]:


question_list.pop()


# In[8]:


question_list


# In[9]:


question_list = [x + separator for x in question_list]


# In[10]:


with open("question_list.pickle", mode="wb") as f:
    pickle.dump(question_list, f)


# In[11]:


with open("question_list.pickle", mode="rb") as f:
    question_list = pickle.load(f)


# In[12]:


#一度吐き出して読み込むことに何の意味があるのかいまいちわからないが、改行がなくなる？？


# In[13]:


print(question_list)


# In[14]:


type(question_list)


# In[15]:


from janome.tokenizer import Tokenizer
from gensim.models import word2vec


# In[16]:


#tokenizerのインスタンス化
t = Tokenizer()


# In[17]:


for sentence in question_list:
    t.tokenize(sentence, wakati=True)


# In[24]:


#表示するだけなのでなくてもいい
for sentence in question_list:
    print(t.tokenize(sentence, wakati=True))


# In[18]:


question_words = []


# In[19]:


for sentence in question_list:
    question_words.append(t.tokenize(sentence, wakati=True))


# In[22]:


##これいらないのかな？
for sentence in question_list:
    question_words += t.tokenize(sentence, wakati=True)


# In[20]:


import collections


# In[21]:


c = collections.Counter(question_words)
print(c)


# In[ ]:





# In[ ]:





# In[22]:


with open ("question_words.pickle", mode="wb") as f:
    pickle.dump(question_words, f)


# In[23]:


with open ("question_words.pickle", mode="rb") as f:
    question_words = pickle.load(f)


# In[182]:


len(question_words)


# In[104]:


print(question_words[233])


# In[26]:


model = word2vec.Word2Vec(question_words,
                size=50,
                min_count=5,
                window=5,
                iter=20,
                sg = 0)


# In[27]:


print(model.wv.vectors.shape)


# In[28]:


print(model.wv.vectors)


# In[29]:


print(len(model.wv.index2word)) #語彙の数


# In[30]:


print(model.wv.index2word)


# In[31]:


print(model.wv.vectors[0])


# In[32]:


print(model.wv.__getitem__("CSV"))


# In[33]:


print(model.wv.most_similar("CSV"))


# In[34]:


print(model.wv.most_similar("データ"))


# In[35]:


model.wv.most_similar(positive=["オープン","CSV"]) #単語ベクトルの演算


# In[36]:


from gensim.models.doc2vec import Doc2Vec
from gensim.models.doc2vec import TaggedDocument


# In[37]:


tagged_documents = []


# In[38]:


#
for i, sentence in enumerate(question_words):
    tagged_documents.append(TaggedDocument(sentence, [i]))


# In[39]:


#表示してみる
print(tagged_documents[36])


# In[300]:


model = Doc2Vec(documents=tagged_documents,
                vector_size=100,
                min_count=5,
                window=5,
                epochs=20,
                dm= 0)


# In[301]:


print(question_words[66])
print(model.docvecs[66])


# In[37]:


print(model.docvecs.most_similar(66))


# In[ ]:





# In[302]:


for p in model.docvecs.most_similar(66):
    print(question_words[p[0]])


# In[303]:


#ここから検証


# In[304]:


vectors_list = [model.docvecs[n] for n in range(len(model.docvecs))]


# In[305]:


doc_nums = range(1, 1+len(model.docvecs))


# In[306]:


kmeans_model = KMeans(n_clusters=20, verbose=1, random_state=1, n_jobs=-1)


# In[307]:


kmeans_model.fit(vectors_list)


# In[308]:


#クラスタリングデータにラベル付け
labels = kmeans_model.labels_


# In[309]:


from collections import defaultdict


# In[310]:


#ラベルとドキュメント番号の辞書づくり
cluster_to_docs = defaultdict(list)
for cluster_id, doc_num in zip(labels, doc_nums):
    cluster_to_docs[cluster_id].append(doc_num)


# In[311]:


#クラスター出力
for j in cluster_to_docs.values():
    print(j)


# In[299]:


print(question_words[32])
print(question_words[41])
print(question_words[87])
print(question_words[191])


# In[312]:


#Noをリスト化
numbers = []
for j in cluster_to_docs.values():
    numbers.append(j)


# In[283]:


#somethingに文章を代入

something2 = [question_words[i[j]] for i in numbers for j in numbers]


# In[292]:


#somethingに文章を代入

something = []
for i in numbers:
    something.append(question_words[i[0]])


# In[293]:


print(something)


# In[56]:


#これで書き出す


# In[58]:


import csv


# In[261]:


with open("question4.csv", mode="w", encoding="CP932") as f:
    writer = csv.writer(f, lineterminator="\n")
    writer.writerows(numbers)


# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:





# In[48]:


some = model.docvecs.most_similar(35)


# In[49]:


#リストをフラットにする関数を作成して適用


# In[50]:


def flatten(nested_list):
    return [e for inner_list in nested_list for e in inner_list]


# In[51]:


some_f = flatten(some)


# In[52]:


#ID部分だけを取り出す


# In[53]:


some_f2 = some_f[0::2]


# In[54]:


print(some_f2)


# In[55]:


#somethingに文章を代入

something = []
for i in some_f2:
    something.append(question_words[i])


# In[56]:


#これで書き出す


# In[58]:


import csv


# In[59]:


with open("question1.csv", mode="w", encoding="CP932") as f:
    writer = csv.writer(f, lineterminator="\n")
    writer.writerows(something)


# In[60]:


#読みこんで確認
with open("question1.csv", mode="r",encoding="CP932") as f:
    something = f.read()


# In[61]:


something_n = re.sub("[,]","",something)   #不要な,の削除


# In[62]:


#再び書き込み
file = open("question2.csv", mode="w",encoding="CP932")
file.write(str(something_n) + "\n")
file.close()


# In[57]:


print(something_n)


# In[209]:


print(dictionary)


# In[ ]:


#ここからクラスタリングの検証


# In[44]:


import gensim


# In[207]:


dictionary = gensim.corpora.Dictionary(question_words)


# In[ ]:


#データの前処理。単語の出現が１以下、あるいは単語が60％以上の文書に登場したとき


# In[208]:


dictionary.filter_extremes(no_below=1,no_above=0.3) 


# In[188]:


corpus = [dictionary.doc2bow(doc) for doc in question_words]


# In[189]:


from gensim.models.ldamodel import LdaModel


# In[ ]:


#LDAのモデルに投入


# In[190]:


lda = LdaModel(corpus, num_topics=10, id2word=dictionary, passes=10)


# In[ ]:


#主題の確認＝トピック集合の内容を表示


# In[191]:


for topic in lda.show_topics(num_topics=-1, num_words=10):
    print('topic id:{0[0]:d}, words={0[1]:s}'.format(topic))


# In[ ]:





# In[ ]:





# In[84]:





# In[192]:


vec_list = []


# In[194]:


for n in range(len(corpus)):
    vec_list.append([lda[corpus[n]][0][1] for i in range(10)])


# In[95]:


lda[corpas[2]][0]


# In[199]:


n


# In[93]:


print(lda[corpus])


# In[78]:


from sklearn.cluster import KMeans, MiniBatchKMeans


# In[98]:


result = KMeans(n_clusters=10).fit_predict(vec_list)


# In[108]:


print(question_words[0:3])


# In[105]:


result


# In[145]:


result2 = list(result)


# In[164]:


len(result2)


# In[198]:


print(corpus)


# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:





# In[ ]:





# In[181]:


import csv
with open("question3.csv", mode="w", encoding="CP932") as f:
    writer = csv.writer(f, lineterminator="\n")
    writer.writerows(question_words)


# In[183]:


#読みこんで確認
with open("question3.csv", mode="r",encoding="CP932") as f:
    something = f.read()


# In[185]:


something


# In[ ]:





# In[151]:


import csv
with open("numbers.csv", mode="w", encoding="CP932") as f:
    writer = csv.writer(f, lineterminator="\n")
    writer.writerow(result2)


# In[ ]:





# In[157]:


#読みこんで確認
with open("numbers.csv", mode="r",encoding="CP932") as f:
    numbers = f.read()


# In[161]:


#読みこんで確認
import pandas as pd
numbers = pd.read_csv("numbers.csv")


# In[162]:


numbers = pd.read_csv("numbers.csv")


# In[168]:


num2 = as.dataframe(numbers)


# In[167]:


texts = pd.read_csv("question1.csv", encoding="CP932")


# In[165]:


#読みこんで確認
with open("question1.csv", mode="r",encoding="CP932") as f:
    texts = f.read()


# In[200]:


import MeCab


# In[ ]:





# In[ ]:





# In[47]:


with open("question1.csv", mode="w", encoding="CP932") as f:
    writer = csv.writer(f, lineterminator="\n")
    writer.writerows(something)


# In[53]:


print(something)


# In[87]:


print(question_words[73])


# In[55]:


type(something)


# In[46]:


type(tagged_documents)


# In[73]:


for p in some_f2:
    some += question_words[p]


# In[50]:


#読みこんで確認
with open("question1.csv", mode="r",encoding="CP932") as f:
    something = f.read()


# In[ ]:


#seriesとかdataframeに変換してみる？
#改行がうまくいかない


# In[62]:


import pandas as pd


# In[64]:


pd.Series(some)


# In[57]:


print(something[:200])


# In[57]:


with open("question1.csv", mode = "r", encoding="CP932")


# In[94]:


for x in enumerate(some_f2):
    senten = question_words[x]


# In[87]:


file = open("sent1.txt", mode="w",encoding="CP932")
file.write(str(sent))
file.close()


# In[83]:


#出力


# In[60]:


import csv
with open("question1.txt", mode="w", encoding="CP932") as f:
    writer = csv.writer(f, lineterminator="\n")
    writer.writerow(question_words[1:10])


# In[90]:


type(sent)


# In[ ]:


for i in some_f2:
    print(question_words[i])


# In[43]:


with open("question11.txt", mode="w", encoding="CP932") as f:
    for something in f:
        f.write(str(something))


# In[153]:


#リストの書き込み
file = open("question_some.csv", mode="w",encoding="CP932")
for x in enumerate(some):
    file.write(str(some) + "\n")
file.close()


# In[44]:


#リストの書き込み
file = open("question_some.csv", mode="w",encoding="CP932")
file.writelines(str(something))
file.close()


# In[115]:


something = re.sub("()","",something)   #全角半角スペース、❘、「」、改行の削除


# In[220]:


type(something)


# In[100]:


str(question_words[66])


# In[101]:


str(some)


# In[117]:


print(question_words[197])


# In[138]:


f = open("question1.txt", mode="w", encoding="CP932")
f.write(question_words[0])
f.close


# In[ ]:


f = open("question.txt", mode="w", encoding="CP932")
for i in 0:len(model.docvecs.most_similar(i)):
        f.write(model.docvecs.most_similar)


# In[ ]:





# In[ ]:





# In[37]:


print(question_list)


# In[ ]:





# In[ ]:




