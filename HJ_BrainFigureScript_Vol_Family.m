clear
addpath('/home/jeonh13/matlab_libraries/gifti-master');
addpath(genpath('/accre/arch/easybuild/software/BinDist/FreeSurfer/6.0.0'));
ProjectFolder = '/labs/h_kaczkurkin_lab/ABCD_FactorAnalysis/figure';

% Create file for surface
[~, VertexLabel_lh, Name_lh] = read_annotation('/accre/arch/easybuild/software/BinDist/FreeSurfer/6.0.0/subjects/fsaverage/label/lh.aparc.annot');
[~, VertexLabel_rh, Name_rh] = read_annotation('/accre/arch/easybuild/software/BinDist/FreeSurfer/6.0.0/subjects/fsaverage/label/rh.aparc.annot');
Vol_Family_Sig = csvread([ProjectFolder '/Vol_Family.csv'], 1);
ind_lh = find(Vol_Family_Sig(:, 1) < 2000);
Vol_Family_Sig_lh = Vol_Family_Sig(ind_lh, :);
ind_rh = find(Vol_Family_Sig(:, 1) > 2000);
Vol_Family_Sig_rh = Vol_Family_Sig(ind_rh, :);
% lh
Vertex_AllLabel_InAtlas_lh = Name_lh.table(2:end, 5);
VertexStatistical_lh = zeros(size(VertexLabel_lh));
Sig_ID = Vol_Family_Sig_lh(:, 1);
for i = 1:length(Sig_ID)
  Vertex_LabelID = Vertex_AllLabel_InAtlas_lh(Sig_ID(i) - 1000);
  VertexIndex = find(VertexLabel_lh == Vertex_LabelID);
  VertexStatistical_lh(VertexIndex) = Vol_Family_Sig_lh(i, 2);
end
V_lh = gifti;
V_lh.cdata = VertexStatistical_lh;
V_lh_File = [ProjectFolder '/Vol_Family_lh_Sig_header.func.gii'];
save(V_lh, V_lh_File);
% rh
Vertex_AllLabel_InAtlas_rh = Name_rh.table(2:end, 5);
VertexStatistical_rh = zeros(size(VertexLabel_rh));
Sig_ID = Vol_Family_Sig_rh(:, 1);
for i = 1:length(Sig_ID)
  Vertex_LabelID = Vertex_AllLabel_InAtlas_rh(Sig_ID(i) - 2000);
  VertexIndex = find(VertexLabel_rh == Vertex_LabelID);
  VertexStatistical_rh(VertexIndex) = Vol_Family_Sig_rh(i, 2);
end
V_rh = gifti;
V_rh.cdata = VertexStatistical_rh;
V_rh_File = [ProjectFolder '/Vol_Family_rh_Sig_header.func.gii'];
save(V_rh, V_rh_File);
