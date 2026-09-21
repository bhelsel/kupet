function old_segmentation(spm_image, grey, white, csf)

matlabbatch = {};

matlabbatch{1}.spm.tools.oldseg.data = {[spm_image, ',1']};

matlabbatch{1}.spm.tools.oldseg.output.GM = [0 0 0];

matlabbatch{1}.spm.tools.oldseg.output.WM = [0 0 0];

matlabbatch{1}.spm.tools.oldseg.output.CSF = [0 0 0];

matlabbatch{1}.spm.tools.oldseg.output.biascor = 0;

matlabbatch{1}.spm.tools.oldseg.output.cleanup = 0;

matlabbatch{1}.spm.tools.oldseg.opts.tpm = {[grey white csf]};

matlabbatch{1}.spm.tools.oldseg.opts.ngaus = [2 2 2 4];

matlabbatch{1}.spm.tools.oldseg.opts.regtype = 'mni';

matlabbatch{1}.spm.tools.oldseg.opts.warpreg = 1;

matlabbatch{1}.spm.tools.oldseg.opts.warpco = 25;

matlabbatch{1}.spm.tools.oldseg.opts.biasreg = 0.0001;

matlabbatch{1}.spm.tools.oldseg.opts.biasfwhm = 60;

matlabbatch{1}.spm.tools.oldseg.opts.samp = 3;

matlabbatch{1}.spm.tools.oldseg.opts.msk = {''};

spm_jobman('run', matlabbatch)

end