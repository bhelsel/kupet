function coregister(ref_image, spm_image)

matlabbatch = {};

matlabbatch{1}.spm.spatial.coreg.estimate.ref = ...
    {[ref_image, ',1']};

matlabbatch{1}.spm.spatial.coreg.estimate.source = ...
    {[spm_image, ',1']};

matlabbatch{1}.spm.spatial.coreg.estimate.other = {''};

matlabbatch{1}.spm.spatial.coreg.estimate.eoptions.cost_fun = 'nmi';

matlabbatch{1}.spm.spatial.coreg.estimate.eoptions.sep = ...
    [4 2];

matlabbatch{1}.spm.spatial.coreg.estimate.eoptions.tol = ...
    [0.02 0.02 0.02 0.001 0.001 0.001 0.01 0.01 0.01 0.001 0.001 0.001];

matlabbatch{1}.spm.spatial.coreg.estimate.eoptions.fwhm = [7 7];

spm_jobman('run', matlabbatch);

end