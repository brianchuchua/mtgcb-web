import styled from 'styled-components';
import IconButton from '@material-ui/core/IconButton';

const HeaderSidenavButton = styled(IconButton)<HeaderSidenavButtonProps>(({ open }) => {
  if (open) {
    return {
      display: 'none',
    };
  }

  return {
    marginRight: 36,
  };
});

interface HeaderSidenavButtonProps {
  open?: boolean;
}

export default HeaderSidenavButton;
