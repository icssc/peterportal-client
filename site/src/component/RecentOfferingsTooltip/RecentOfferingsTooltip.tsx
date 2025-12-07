import { FC } from 'react';
import './RecentOfferingsTooltip.scss';
import { Typography } from '@mui/material';
import RecentOfferingsTable from '../RecentOfferingsTable/RecentOfferingsTable';
import OverlayTrigger from '../OverlayTrigger/OverlayTrigger';

interface RecentOfferingsTooltipProps {
  terms: string[];
}

const RecentOfferingsTooltip: FC<RecentOfferingsTooltipProps> = ({ terms }) => {
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

  return (
    <OverlayTrigger
      popoverContent={
        <div className="popover-body">
          <h4 className="center">Recent Offerings</h4>
          <RecentOfferingsTable terms={terms} size="thin" />
        </div>
      }
      anchor="left"
      transform="right"
    >
      <Typography>{prevOfferings}</Typography>
    </OverlayTrigger>
  );
};

export default RecentOfferingsTooltip;
