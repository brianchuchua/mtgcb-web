import styled from 'styled-components';
import IconButton from '@material-ui/core/IconButton';

const HeaderSidenavButton = styled(IconButton)(({ open }) => {
  if (open) {
    return {
      display: 'none',
    };
  }

  return {
    marginRight: 36,
  };
});

export default HeaderSidenavButton;
