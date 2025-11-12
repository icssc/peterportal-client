import { FC, useState, MouseEvent } from 'react';
import './RecentOfferingsTooltip.scss';
// import { OverlayTrigger, Popover } from 'react-bootstrap';
import { Popover, Typography } from '@mui/material';
import RecentOfferingsTable from '../RecentOfferingsTable/RecentOfferingsTable';

interface RecentOfferingsTooltipProps {
  terms: string[];
}

const RecentOfferingsTooltip: FC<RecentOfferingsTooltipProps> = ({ terms }) => {
  const [anchorEl, setAnchorEl] = useState<HTMLElement | null>(null);

  if (terms.length === 0) return null;

  const prevYear = new Date().getMonth() > 9 ? new Date().getFullYear() : new Date().getFullYear() - 1;
  const prevOfferings = [];

  // if the course was offered in the previous academic year, add the corresponding emoji to prevOfferings
  if (terms.includes(`${prevYear - 1} Fall`)) prevOfferings.push('🍂');
  if (terms.includes(`${prevYear} Winter`)) prevOfferings.push('❄️');
  if (terms.includes(`${prevYear} Spring`)) prevOfferings.push('🌸');
  if (terms.some((t) => t.startsWith(`${prevYear} Summer`))) prevOfferings.push('☀️');

  // if the course was not offered in the previous academic year, show a ❌
  if (prevOfferings.length === 0) prevOfferings.push('❌');

  // const popover = (
  //   <Popover id="recent-offerings-popover" className="ppc-popover recent-offerings-popover">
  //     <div className="popover-body">
  //       <h4 className="center">Recent Offerings</h4>
  //       <RecentOfferingsTable terms={terms} size="thin" />
  //     </div>
  //   </Popover>
  // );

  const handlePopoverOpen = (event: MouseEvent<HTMLElement>) => {
    setAnchorEl(event.currentTarget);
  };

  const handlePopoverClose = () => {
    setAnchorEl(null);
  };

  const open = Boolean(anchorEl);

  return (
    // <OverlayTrigger overlay={popover} placement="auto">
    //   <div className="tooltip-trigger">
    //     {prevOfferings.map((emoji) => (
    //       <span key={emoji}>{emoji}</span>
    //     ))}
    //   </div>
    // </OverlayTrigger>
    <div className="tooltip-trigger">
      <Typography
        aria-owns={open ? 'recent-offerings-popover' : undefined}
        onMouseEnter={handlePopoverOpen}
        onMouseLeave={handlePopoverClose}
      >
        {prevOfferings}
      </Typography>
      <Popover
        id="recent-offerings-popover"
        className="ppc-popover recent-offerings-popover"
        sx={{ pointerEvents: 'none' }}
        open={open}
        anchorEl={anchorEl}
        anchorOrigin={{
          vertical: 'center',
          horizontal: 'left',
        }}
        transformOrigin={{
          vertical: 'center',
          horizontal: 'right',
        }}
        onClose={handlePopoverClose}
        disableRestoreFocus
      >
        <div className="popover-body">
          <h4 className="center">Recent Offerings</h4>
          <RecentOfferingsTable terms={terms} size="thin" />
        </div>
      </Popover>
    </div>
  );
};

export default RecentOfferingsTooltip;
