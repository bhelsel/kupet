function reslice_mean(pet_files, output_dir, output_file)

pet_files = strcat(strsplit(pet_files, '|'), ',1');

matlabbatch = {};

matlabbatch{1}.spm.spatial.realign.write.data = {pet_files'};

matlabbatch{1}.spm.spatial.realign.write.roptions.which = [2 0];

matlabbatch{1}.spm.spatial.realign.write.roptions.interp = 0;

matlabbatch{1}.spm.spatial.realign.write.roptions.wrap = [0 0 0];

matlabbatch{1}.spm.spatial.realign.write.roptions.mask = 1;

matlabbatch{1}.spm.spatial.realign.write.roptions.prefix = 'r';

matlabbatch{2}.spm.util.imcalc.input(1) = cfg_dep('Realign: Reslice: Resliced Images', substruct('.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','rfiles'));

matlabbatch{2}.spm.util.imcalc.output = output_file;

matlabbatch{2}.spm.util.imcalc.outdir = {output_dir};

matlabbatch{2}.spm.util.imcalc.expression = '(i1+i2+i3+i4)/4';

matlabbatch{2}.spm.util.imcalc.var = struct('name', {}, 'value', {});

matlabbatch{2}.spm.util.imcalc.options.dmtx = 0;

matlabbatch{2}.spm.util.imcalc.options.mask = 0;

matlabbatch{2}.spm.util.imcalc.options.interp = 1;

matlabbatch{2}.spm.util.imcalc.options.dtype = 4;

spm_jobman('run', matlabbatch)

end