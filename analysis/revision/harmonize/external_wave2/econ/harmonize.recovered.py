# Writes harmonized CSVs. Prints only counts and column names, never text values.
import site, sys; sys.path.append(site.getusersitepackages())
import pandas as pd
P, S = sys.argv[1], sys.argv[2]
import os; O = os.environ.get('HARMONIZE_OUT', P)  # path changed on recovery (2026-11-17): output root (default: as originally)
def nz(s): return s.fillna('').astype(str).str.strip().replace({'nan':''})
def out(df, path):
    df = df[nz(df['text'])!='']
    df.to_csv(path, index=False, encoding='utf-8')
    txt = open(path, encoding='utf-8').read()
    print(path.split('/')[-1], 'rows=', len(df), 'cols=', list(df.columns), 'treat=', df['treatment'].value_counts().to_dict(), 'excl=', int(df['exclude'].sum()), 'U+count=', txt.count('<U+'), 'ids_unique=', df['id'].is_unique)
# 1 news
R = P + '/chopra2024_newsdemand/raw/'; C = S + '/news/x/MS20260707-replication/3 replication package/data/'
hc = pd.read_stata(R + 'open_ended_reason.dta')[['prolific_pid','classification']].drop_duplicates('prolific_pid')
parts = []
for k in ['1','2']:
    d = pd.read_stata(R + f'study{k}.dta', convert_categoricals=False)
    keep = set(pd.read_stata(C + f'study{k}_cleaned.dta', columns=['prolific_pid'])['prolific_pid'])
    d = d[d['treatment'].isin(['biased','unbiased'])].copy()
    t = nz(d['reason_yes']) + ' | ' + nz(d['reason_no'])
    d['text'] = t.str.strip(' |')
    d['id'] = 'exp' + k + '_' + d['responseid'].astype(str)
    d['treatment'] = (d['treatment']=='biased').astype(int)
    d['exclude'] = ~d['prolific_pid'].isin(keep)
    d = d.merge(hc, on='prolific_pid', how='left').rename(columns={'classification':'human_bias_motive_class'})
    print('exp',k,'in cleaned sample:', int((~d['exclude']).sum()), 'human code merged:', int(d['human_bias_motive_class'].notna().sum()))
    parts.append(d)
d = pd.concat(parts)
d['dataset']='chopra2024_newsdemand'; d['country']='US'; d['lang']='en'
out(d[['id','dataset','country','lang','text','treatment','exclude','human_bias_motive_class']], O + '/chopra2024_newsdemand/harmonized_chopra2024_newsdemand.csv')  # path changed on recovery (2026-11-17)
# 2 SSM
R = P + '/graeber2024_storiesmemory/raw/'
x = pd.read_excel(R + 'BaselineExperiment_coded_recall_merged.xlsx')
codes = list(x.columns[x.columns.get_loc('Nomemory_a'):])
print('SSM all cols:', list(x.columns))
x = x[x['condition_'].astype(str).str.startswith(('storyshort','statistic'))].copy()
x['treatment'] = x['condition_'].astype(str).str.startswith('storyshort').astype(int)
x['id'] = x['prolific_pid'].astype(str) + '_' + x['product'].astype(str)
x['text'] = x['free_form']; x['exclude'] = False
x['dataset']='graeber2024_storiesmemory'; x['country']='US'; x['lang']='en'
x = x.rename(columns={c:'human_'+c for c in codes})
out(x[['id','dataset','country','lang','text','treatment','exclude']+['human_'+c for c in codes]], O + '/graeber2024_storiesmemory/harmonized_graeber2024_storiesmemory.csv')  # path changed on recovery (2026-11-17)
# 3 narratives attention
R = P + '/andre2025_macronarratives/raw/'
a = pd.read_stata(R + 'attention_experiment.dta', convert_categoricals=False)
idc = [c for c in ['responseid','prolific_pid','response_id','id'] if c in a.columns]
print('narr id candidates:', idc)
a = a[a['treatment'].isin(['baseline','prime_govspending'])].copy()
a['id'] = 'att_' + a[idc[0]].astype(str)
a['treatment'] = (a['treatment']=='prime_govspending').astype(int)
a['text'] = a['story']; a['exclude'] = False
a['dataset']='andre2025_macronarratives'; a['country']='US'; a['lang']='en'
out(a[['id','dataset','country','lang','text','treatment','exclude']], O + '/andre2025_macronarratives/harmonized_andre2025_macronarratives.csv')  # path changed on recovery (2026-11-17)
