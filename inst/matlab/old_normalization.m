function old_normalization(seg_mat, mri, pet)

matlabbatch = {};

matlabbatch{1}.spm.tools.oldnorm.write.subj(1).matname = {seg_mat};

matlabbatch{1}.spm.tools.oldnorm.write.subj(1).resample = {
    sprintf('%s,1', mri)
    sprintf('%s,1', pet)
    };

matlabbatch{1}.spm.tools.oldnorm.write.roptions.preserve = 0;

matlabbatch{1}.spm.tools.oldnorm.write.roptions.bb = ...
    [NaN NaN NaN
    NaN NaN NaN];

matlabbatch{1}.spm.tools.oldnorm.write.roptions.vox = [2 2 2];

matlabbatch{1}.spm.tools.oldnorm.write.roptions.interp = 1;

matlabbatch{1}.spm.tools.oldnorm.write.roptions.wrap = [0 0 0];

matlabbatch{1}.spm.tools.oldnorm.write.roptions.prefix = 'w';

spm_jobman('run', matlabbatch)

end