import { useEffect, useState } from 'react';
import { Button, IconButton } from '@mui/material';
import ArticleOutlinedIcon from '@mui/icons-material/ArticleOutlined';
import BarChartIcon from '@mui/icons-material/BarChart';
import CalendarTodayIcon from '@mui/icons-material/CalendarToday';
import KeyboardArrowDownIcon from '@mui/icons-material/KeyboardArrowDown';
import KeyboardArrowUpIcon from '@mui/icons-material/KeyboardArrowUp';
import RateReviewIcon from '@mui/icons-material/RateReview';

const previewLinks = [
  { id: 'preview-details', label: 'Details', icon: <ArticleOutlinedIcon /> },
  { id: 'preview-grades', label: 'Grades', icon: <BarChartIcon /> },
  { id: 'preview-schedule', label: 'Schedule', icon: <CalendarTodayIcon /> },
  { id: 'preview-reviews', label: 'Reviews', icon: <RateReviewIcon /> },
];

const useActivePreviewSection = () => {
  const [activeSection, setActiveSection] = useState(previewLinks[0].id);

  useEffect(() => {
    const scrollContainer = document.querySelector('.result-preview > div:last-child');
    if (!scrollContainer) return;

    const updateActiveSection = () => {
      const containerTop = scrollContainer.getBoundingClientRect().top;
      let currentSection = previewLinks[0];

      for (let i = previewLinks.length - 1; i >= 0; i--) {
        const section = document.getElementById(previewLinks[i].id);
        if (section && section.getBoundingClientRect().top <= containerTop + 40) {
          currentSection = previewLinks[i];
          break;
        }
      }

      setActiveSection(currentSection.id);
    };

    updateActiveSection();
    scrollContainer.addEventListener('scroll', updateActiveSection);
    return () => scrollContainer.removeEventListener('scroll', updateActiveSection);
  }, []);

  return activeSection;
};

export const PreviewArrowNav = () => {
  const activeSection = useActivePreviewSection();
  const activeIndex = previewLinks.findIndex((link) => link.id === activeSection);

  const scrollToSection = (id: string) => {
    document.getElementById(id)?.scrollIntoView({ behavior: 'smooth', block: 'start' });
  };

  return (
    <div className="preview-arrow-nav">
      <IconButton
        disabled={activeIndex <= 0}
        disableRipple={true}
        onClick={() => scrollToSection(previewLinks[activeIndex - 1].id)}
      >
        <KeyboardArrowUpIcon />
      </IconButton>
      <div className="preview-arrow-divider" />
      <IconButton
        disabled={activeIndex >= previewLinks.length - 1}
        disableRipple={true}
        onClick={() => scrollToSection(previewLinks[activeIndex + 1].id)}
      >
        <KeyboardArrowDownIcon />
      </IconButton>
    </div>
  );
};

const PreviewNavBar = () => {
  const activeSection = useActivePreviewSection();

  const scrollToSection = (id: string) => {
    document.getElementById(id)?.scrollIntoView({ behavior: 'smooth', block: 'start' });
  };

  return (
    <nav className="preview-nav-bar" aria-label="Preview sections">
      {previewLinks.map((link) => (
        <Button
          className={`preview-nav-button ${activeSection === link.id ? 'active' : ''}`}
          color="inherit"
          key={link.id}
          onClick={() => scrollToSection(link.id)}
          size="small"
          startIcon={link.icon}
          variant="contained"
          disableRipple={true}
        >
          <span>{link.label}</span>
        </Button>
      ))}
    </nav>
  );
};

export default PreviewNavBar;
