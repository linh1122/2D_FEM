K = readmatrix('./data/matrix/K_NON_BOUNDARY.dat');
F = readmatrix('./data/matrix/F_NON_BOUNDARY.dat');

innerNode     = readmatrix('./data/matrix/NON_BOUNDARY_NODE_NUMBER.dat');
nodeCondition = readmatrix('./mesh/boundary.dat');

TOnBoundary   = readmatrix('./data/matrix/T_ON_BOUNDARY.dat');

nodes = readmatrix('./mesh/nodes.dat');

dimensions = readmatrix('./mesh/dimensions.dat');

nn = readmatrix('./mesh/nodes_number.dat');

count = 0;
nodesNumber = zeros(dimensions(1,1),dimensions(1,2));
for i=1:dimensions(1,1)
    for j=1:dimensions(1,2)
        count = count + 1;
        nodesNumber(i,j) = nn(1,count);
    end
end

F_t = transpose(F);
T = K\F_t;

numberOfNodes = size(nodeCondition,1)

size(nodes)
size(T)
size(nn)

T_final = zeros(numberOfNodes,1);
count = 0;
for i=1:numberOfNodes
    if(nodeCondition(i,2)~=0) 
        T_final(i,1)=TOnBoundary(nodeCondition(i,2));
    else
        count = count + 1;
        T_final(i,1)=T(count,1);
    end
end

fileID = fopen("./data/result.tec",'w');
fprintf(fileID,'%12s %12s %12s %12s\r\n','VARIABLES = ','X', 'Y', 'T');
fprintf(fileID,'%12s %12i %12s %12i %12s\r\n','ZONE  I=',dimensions(1,1),',J=',dimensions(1,2),', F=POINT');
for j=1:dimensions(1,2)
    for i=1:dimensions(1,1)
        fprintf(fileID,'%12f %12f %12f\r\n',nodes(nodesNumber(i,j),1),nodes(nodesNumber(i,j),2),T_final(nodesNumber(i,j),1));
    end
end
fclose(fileID);



