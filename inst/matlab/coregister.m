function coregister(ref_image, source_image, other_image)

matlabbatch = {};

matlabbatch{1}.spm.spatial.coreg.estimate.ref = ...
    {[ref_image, ',1']};

matlabbatch{1}.spm.spatial.coreg.estimate.source = ...
    {[source_image, ',1']};

if ~isempty(other_image)
    matlabbatch{1}.spm.spatial.coreg.estimate.other = ...
        {[other_image, ',1']};
else
    matlabbatch{1}.spm.spatial.coreg.estimate.other = {''};
end

matlabbatch{1}.spm.spatial.coreg.estimate.eoptions.cost_fun = 'nmi';

matlabbatch{1}.spm.spatial.coreg.estimate.eoptions.sep = ...
    [4 2];

matlabbatch{1}.spm.spatial.coreg.estimate.eoptions.tol = ...
    [0.02 0.02 0.02 0.001 0.001 0.001 ...
    0.01 0.01 0.01 0.001 0.001 0.001];

matlabbatch{1}.spm.spatial.coreg.estimate.eoptions.fwhm = [7 7];

spm_jobman('run', matlabbatch);

end