input = "tidy_cb_data.csv"
output = "tidy_cb_data_norep.csv"

with open(input, 'r') as f:
    data = f.readlines()
    
new_data = ''

for i, line in enumerate(data,start = 1):
    if (i%2 == 0):
        new_data += line

with open(output, 'w') as f:
    f.write(new_data)