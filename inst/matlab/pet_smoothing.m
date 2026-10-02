function pet_smoothing(pet)

matlabbatch = {};

matlabbatch{1}.spm.spatial.smooth.data = {pet};

matlabbatch{1}.spm.spatial.smooth.fwhm = [2 2 2];

matlabbatch{1}.spm.spatial.smooth.dtype = 0;

matlabbatch{1}.spm.spatial.smooth.im = 0;

matlabbatch{1}.spm.spatial.smooth.prefix = 's';

spm_jobman('run', matlabbatch)

end