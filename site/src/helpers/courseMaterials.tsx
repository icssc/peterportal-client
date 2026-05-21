import AttachMoneyIcon from '@mui/icons-material/AttachMoney';
import SouthOutlinedIcon from '@mui/icons-material/SouthOutlined';
import Link from 'next/link';

export default function MaterialsIcon() {
  const libraryLink = 'https://www.lib.uci.edu/affordable-initiatives/course-materials';

  const iconStyle: React.CSSProperties = {
    position: 'relative',
    width: 24,
    height: 24,
    flexShrink: 0,
    color: 'var(--mui-palette-success-light)',
  };

  return (
    <Link href={libraryLink} rel="noopener noreferrer" target="_blank">
      <div className="materials-icon" style={iconStyle}>
        <AttachMoneyIcon />
        <SouthOutlinedIcon
          sx={{
            fontSize: '15px',
            position: 'absolute',
            bottom: '-4px',
            right: '-2px',
          }}
        />
      </div>
    </Link>
  );
}
