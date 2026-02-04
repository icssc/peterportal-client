import { FC, useState, useEffect } from 'react';
import './ScrollToTopButton.scss';
import IconButton from '@mui/material/IconButton';
import KeyboardArrowUpIcon from '@mui/icons-material/KeyboardArrowUp';

interface ScrollToTopButtonProps {
  scrollableTarget: string;
  raiseButton?: boolean;
}

const ScrollToTopButton: FC<ScrollToTopButtonProps> = ({ scrollableTarget, raiseButton = false }) => {
  const [visible, setVisible] = useState(false);

  useEffect(() => {
    const el = document.getElementById(scrollableTarget);
    if (!el) return;

    const onScroll = () => {
      setVisible(el.scrollTop > 300);
    };

    el.addEventListener('scroll', onScroll);
    return () => el.removeEventListener('scroll', onScroll);
  }, [scrollableTarget]);

  const jumpToTop = () => {
    document.getElementById(scrollableTarget)?.scrollTo({ top: 0, behavior: 'smooth' });
  };

  if (!visible) return null;

  return (
    <IconButton
      onClick={jumpToTop}
      className="scroll-to-top-button"
      size="large"
      sx={{
        backgroundColor: 'primary.main',
        color: 'primary.contrastText',
        '&:hover': {
          backgroundColor: 'primary.dark',
        },
        bottom: raiseButton ? '4rem' : '1.5rem',
      }}
    >
      <KeyboardArrowUpIcon />
    </IconButton>
  );
};

export default ScrollToTopButton;
