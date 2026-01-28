import { FC, useState, useEffect } from 'react';
import './ScrollToTopButton.scss';
import Button from '@mui/material/Button';

interface ScrollToTopButtonProps {
  scrollableTarget: string;
}

const ScrollToTopButton: FC<ScrollToTopButtonProps> = ({ scrollableTarget }) => {
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
    <Button onClick={jumpToTop} className="scroll-to-top-button">
      Jump to Top
    </Button>
  );
};

export default ScrollToTopButton;
