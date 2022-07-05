/* eslint-disable @typescript-eslint/ban-ts-comment */
// @ts-nocheck - I need to be able to use the MuiSkeleton class even though it's not defined in the typing of this library.
import { indigo } from '@material-ui/core/colors';
import { createMuiTheme } from '@material-ui/core/styles';
import breakpoints from './breakpoints';

const darkTheme = createMuiTheme({
  props: {
    MuiInputBase: {
      inputProps: {
        spellCheck: 'false',
        autoCapitalize: 'off',
        autoCorrect: 'off',
        autoComplete: 'off',
      },
    },
    MuiInput: {
      inputProps: {
        spellCheck: 'false',
        autoCapitalize: 'off',
        autoCorrect: 'off',
        autoComplete: 'off',
      },
    },
  },
  palette: {
    type: 'dark',
    primary: { main: '#8a85ff' },
    background: {
      paper: '#282C34',
      default: '#1c2025',
    },
    secondary: indigo,
  },
  breakpoints: {
    values: breakpoints,
  },
  overrides: {
    MuiCssBaseline: {
      '@global': {
        '.input-label-fix.MuiInputLabel-outlined.MuiInputLabel-shrink': {
          transform: 'translate(22px, -6px) scale(0.75)', // Fix for Material UI bug with labels on outlined inputs
        },
        '*': {
          'scrollbar-width': '10px',
          'scrollbar-color': '#474c50',
        },
        '*::-webkit-scrollbar': {
          width: '10px',
        },

        '*::-webkit-scrollbar-track': {
          'background-color': 'rgba(202,204,206,0.04)',
        },

        '*::-webkit-scrollbar-thumb': {
          'background-color': '#474c50',
          'border-radius': '10px',
        },
        '*::-webkit-scrollbar-corner': {
          'background-color': '#474c50',
        },
      },
    },
    MuiPaper: {
      outlined: {
        borderColor: 'rgba(255, 255, 255, 0.23)',
      },
    },
    MuiSkeleton: {
      root: {
        backgroundColor: '#282C34',
      },
    },
  },
});

export default darkTheme;
