/* eslint-disable jsx-a11y/no-static-element-interactions */
/* eslint-disable jsx-a11y/click-events-have-key-events */
import FormControl from '@material-ui/core/FormControl';
import InputLabel from '@material-ui/core/InputLabel';
import MenuItem from '@material-ui/core/MenuItem';
import Select from '@material-ui/core/Select';
import { useEffect, useState } from 'react';
import { animateScroll as scroll, scroller } from 'react-scroll';

interface JumpToSelectProps {
  label: string;
  goToOptions?: { label: string; value: string }[];
  isMobile?: boolean;
}
const JumpToSelect: React.FC<JumpToSelectProps> = ({ label, goToOptions, isMobile = false }) => {
  const [selectedSlug, setSelectedSlug] = useState<string | undefined>(undefined);

  useEffect(() => {
    if (goToOptions && goToOptions.length > 0) {
      setSelectedSlug(goToOptions[0].value);
    }
  }, [goToOptions?.[0]]);

  if (!goToOptions || goToOptions?.length === 0 || !selectedSlug) return null;

  return (
    <FormControl>
      <InputLabel id="jump-to-label">{label}</InputLabel>
      <Select
        labelId="jump-to-label"
        id="jump-to-select-select"
        label="Go to"
        value={selectedSlug}
        onChange={(e) => {
          setSelectedSlug(e.target.value as string);
        }}
        style={{ marginRight: '10px' }}
      >
        {goToOptions.map((option, index) => (
          <MenuItem
            key={option.value}
            value={option.value}
            onClick={() => {
              if (index === 0) {
                scroll.scrollToTop({
                  duration: 500,
                  delay: 0,
                  smooth: 'easeInOutQuart',
                });
                return;
              }
              if (isMobile) {
                scroller.scrollTo(`anchor-link-${option.value}`, {
                  duration: 250,
                  delay: 0,
                  smooth: 'easeInOutQuart',
                  offset: -62,
                });
                scroller.scrollTo(`anchor-link-${option.value}`, {
                  duration: 250,
                  delay: 250,
                  smooth: 'easeInOutQuart',
                  offset: -62,
                });
              } else {
                scroller.scrollTo(`anchor-link-${option.value}`, {
                  duration: 500,
                  delay: 0,
                  smooth: 'easeInOutQuart',
                });
                // This gives images time to load so that the scroll can calculate the correct position
                setTimeout(() => {
                  scroller.scrollTo(`anchor-link-${option.value}`, {
                    duration: 500,
                    delay: 0,
                    smooth: 'easeInOutQuart',
                  });
                }, 500);
              }
            }}
          >
            {option.label}
          </MenuItem>
        ))}
      </Select>
    </FormControl>
  );
};
export default JumpToSelect;
