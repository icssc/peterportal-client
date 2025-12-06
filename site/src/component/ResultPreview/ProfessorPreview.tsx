import { FC, ReactNode, useEffect, useState } from 'react';
import { ResultPageSection } from '../ResultPageContent/ResultPageContent';
import GradeDist from '../GradeDist/GradeDist';
import Schedule from '../Schedule/Schedule';
import Review from '../Review/Review';
import LoadingSpinner from '../LoadingSpinner/LoadingSpinner';
import { checkModalOpen, sortTerms, transformProfessorGQL, unionTerms } from '../../helpers/util';
import { ProfessorGQLData } from '../../types/types';
import { Button, Fade, IconButton, Paper, Tooltip, useMediaQuery, useTheme } from '@mui/material';
import { useAppDispatch } from '../../store/hooks';
import { setPreviewedProfessor } from '../../store/slices/coursePreviewSlice';
import { setToastMsg, setToastSeverity, setShowToast } from '../../store/slices/roadmapSlice';
import Twemoji from 'react-twemoji';

import CloseIcon from '@mui/icons-material/Close';
import IosShareIcon from '@mui/icons-material/IosShare';
import { getProfessorTerms } from '../../helpers/reviews';
import SideInfo from '../SideInfo/SideInfo';
import trpc from '../../trpc';

/** @todo remove once cached professors gets merged */
function useProfessorData(ucinetid: string) {
  const [fullProfData, setFullProfData] = useState<ProfessorGQLData | null>(null);
  const [loadTrigger, setLoadTrigger] = useState(false);

  const dispatch = useAppDispatch();

  useEffect(() => {
    // Use a stateful trigger to avoid sending two requests as a result of double first render
    setLoadTrigger(true);
  }, [ucinetid]);

  /** @todo read from global cache */
  useEffect(() => {
    if (!loadTrigger) return;
    setLoadTrigger(false);
    setFullProfData(null);
    trpc.professors.get
      .query({ ucinetid })
      .then((prof) => {
        setFullProfData(transformProfessorGQL(prof));
      })
      .catch(() => {
        dispatch(setToastMsg('Unable to load professor'));
        dispatch(setToastSeverity('error'));
        dispatch(setShowToast(true));
      });
  }, [ucinetid, dispatch, loadTrigger]);

  return fullProfData;
}

interface PreviewTitleProps {
  isLoading: boolean;
  netId: string;
  professorData: ProfessorGQLData | null;
}
const PreviewTitle: FC<PreviewTitleProps> = ({ isLoading, netId, professorData }) => {
  const wrapContent = (content: ReactNode) => <p className="preview-title">{content}</p>;
  const shortenText = useMediaQuery('(max-width: 480px)');

  if (isLoading || !professorData) {
    const loadingText = shortenText ? 'Loading...' : `Loading ${netId}...`;
    return wrapContent(loadingText);
  }

  const formattedCourseId = <b>{professorData.name}</b>;
  if (shortenText) {
    return wrapContent(formattedCourseId);
  }

  return wrapContent(<>Previewing {formattedCourseId}</>);
};

const ProfessorPreviewContent: FC<{ data: ProfessorGQLData | null }> = ({ data }) => {
  if (!data) {
    return <LoadingSpinner />;
  }

  return (
    <div className="preview-body">
      <SideInfo
        className="professor-summary"
        searchType="professor"
        name={data.name}
        title={data.title}
        description={data.department}
        tags={[data.ucinetid, ...data.shortenedNames]}
        professor={data}
      />

      <ResultPageSection title="📊 Grade Distribution">
        <GradeDist professor={data} />
      </ResultPageSection>

      <ResultPageSection title="🗓️ Schedule of Classes">
        <Schedule professorIDs={data.shortenedNames} termsOffered={unionTerms(data.courses)} />
      </ResultPageSection>

      <ResultPageSection title="💬 Reviews">
        <Review professor={data} terms={sortTerms(getProfessorTerms(data))} />
      </ResultPageSection>
    </div>
  );
};

const ProfessorPreview: FC<{ netid: string }> = ({ netid }) => {
  netid = netid.replace(/\s/g, '');
  const professorData = useProfessorData(netid);
  const isLoading = false;
  const dispatch = useAppDispatch();

  const [open, setOpen] = useState(true);
  const theme = useTheme();
  const transitionTime = theme.transitions.duration.shortest;

  const closePreview = () => {
    setOpen(false);
    setTimeout(() => {
      dispatch(setPreviewedProfessor(''));
    }, transitionTime);
  };

  const copyProfLink = () => {
    const url = new URL('/professor/' + netid, location.origin).toString();
    navigator.clipboard.writeText(url);
    dispatch(setToastMsg('Copied course URL to clipboard!'));
    dispatch(setToastSeverity('success'));
    dispatch(setShowToast(true));
  };

  useEffect(() => {
    const listener = (event: KeyboardEvent) => {
      const modified = event.altKey || event.shiftKey || event.ctrlKey || event.metaKey;
      if (event.key !== 'Escape' || modified) return;
      if (checkModalOpen()) return;
      event.preventDefault();
      closePreview();
    };

    document.body.addEventListener('keydown', listener);
    return () => document.body.removeEventListener('keydown', listener);
  });

  return (
    <Fade in={open} timeout={{ enter: 0, exit: transitionTime }}>
      <div className="result-preview">
        <Paper className="preview-header" variant="outlined">
          <Tooltip title="Exit Preview (Esc)">
            <IconButton onClick={closePreview}>
              <CloseIcon />
            </IconButton>
          </Tooltip>
          <PreviewTitle isLoading={isLoading} netId={netid} professorData={professorData} />
          <Button
            variant="contained"
            color="inherit"
            startIcon={<IosShareIcon />}
            size="small"
            disableElevation
            onClick={copyProfLink}
          >
            Share
          </Button>
        </Paper>
        <Twemoji options={{ className: 'twemoji' }}>
          <ProfessorPreviewContent data={professorData} />
        </Twemoji>
      </div>
    </Fade>
  );
};

export default ProfessorPreview;
